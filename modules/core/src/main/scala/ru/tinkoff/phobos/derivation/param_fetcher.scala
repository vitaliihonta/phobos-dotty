package ru.tinkoff.phobos.derivation

import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}
import scala.quoted.{given _, _}
import scala.quoted.show._
import scala.tasty.reflect._
import scala.tasty.Reflection
import ru.tinkoff.phobos.syntax

case class CaseClassParam (r: Reflection)(
  localName: String,
  xmlName: String,
  namespaceUri: String,
  paramType: r.Type,
  category: ParamCategory
):

  override def toString: String = s"CaseClassParam($localName, $xmlName, $namespaceUri, $paramType, $category)"

end CaseClassParam

// final case class SealedTraitSubtype(
//   constructorName: Reflection#Tree,
//   subtypeType: Reflection#Type
// )

object FetchGroup {
  private def raiseError(msg: =>String)(using q: QuoteContext): Nothing = {
    q.tasty.error(msg, q.tasty.rootPosition)
    ???
  }
  def fetchProduct[T: Type](using q: QuoteContext): List[CaseClassParam] = 
    import q.tasty._
    val t = typeOf[T]
    val classSym: Symbol = t.classSymbol getOrElse raiseError(s"$t should be a class")
    val attrType = typeOf[syntax.attr]
    val textType = typeOf[syntax.text]
    
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
      val params = classSym.caseFields.map:
        field => 
          val (annotations, tpe) = field.tree match
            case valDef@ValDef(_, _, annotations) =>
              annotations -> valDef.tpt.tpe

          CaseClassParam(q.tasty)(
            localName = field.name,
            xmlName = field.name,
            namespaceUri = "ns",
            paramType = tpe,
            category = fetchGroup(field)
          )

      params
  end fetchProduct


  def tmp[T: Type](using QuoteContext): Expr[String] = 
    val params = fetchProduct[T].mkString("\n", ",\n", "\n")
    Expr(params)
  
  inline def get[T]: String = 
    ${tmp[T]}
  end get
}