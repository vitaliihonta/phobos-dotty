package ru.tinkoff.phobos.derivation

import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}
import scala.quoted.{given _, _}
import scala.quoted.show._
import scala.tasty.reflect._
import ru.tinkoff.phobos.syntax

object FetchGroup {
  def fetchProduct[T: Type](using ops: QuotedOps): List[ops.CaseClassParam] = 
    given QuoteContext = ops.quoteContext
    import ops.quoteContext.tasty._
    val t = typeOf[T]
    val classSym: Symbol = t.classSymbol getOrElse raiseError(s"$t should be a class")
    val attrType = typeOf[syntax.attr]
    val textType = typeOf[syntax.text]
    val renamedType = typeOf[syntax.renamed]
    
    if classSym.caseFields.isEmpty then 
      raiseError(s"$t shoud be a case class with fields")
    else 
      def fetchGroup(param: Symbol): ParamCategory = 
        val p = param.annots.foldLeft[List[ParamCategory]](Nil):
          (acc, annotation) =>
            annotation.tpe match
              case tpe if tpe =:= attrType => ParamCategory.attribute :: acc
              case tpe if tpe =:= textType => ParamCategory.text :: acc
              case _                      => acc
        p match
          case List(group) => group
          case Nil         => ParamCategory.element
          case groups =>
            raiseError(s"Parameter ${param.name} must not have multiple xml annotations (${groups.mkString(", ")})")
      end fetchGroup

      def fetchXmlName(param: Symbol): String = 
        param.annots.collectFirst {
          case annot@Apply(_, List(Literal(Constant(renamed: String)))) if annot.tpe =:= renamedType =>
            renamed
        } getOrElse param.name

      val params = classSym.caseFields.map:
        field => 
          val (annotations, tpe) = field.tree match
            case valDef@ValDef(_, _, annotations) =>
              annotations -> valDef.tpt.tpe

          ops.CaseClassParam(
            localName = field.name,
            xmlName = fetchXmlName(field),
            namespaceUri = "ns",
            paramType = tpe,
            category = fetchGroup(field)
          )
      val attributeParamsNumber = params.count(_.category == ParamCategory.attribute)
      val regularParamsNumber   = params.count(_.category == ParamCategory.element)
      val textParamsNumber      = params.count(_.category == ParamCategory.text)

      (attributeParamsNumber, regularParamsNumber, textParamsNumber) match
        case (_, _, l) if l > 1 => raiseError(s"Multiple @text parameters in one class")
        case _                  => params
  end fetchProduct


  def tmp[T: Type](using q: QuoteContext): Expr[String] = 
    given QuotedOps = QuotedOps(q)
    val params = fetchProduct[T].mkString("\n", ",\n", "\n")
    Expr(params)
  
  inline def get[T]: String = 
    ${tmp[T]}
  end get
}