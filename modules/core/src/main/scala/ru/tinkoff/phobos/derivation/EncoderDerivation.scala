package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.encoding._
import scala.quoted.{Expr, Type, QuoteContext}

object EncoderDerivation:
  def deriveImpl[T](using t: Type[T], q: QuoteContext): Expr[ElementEncoder[T]] =
    given ops as QuotedOps = QuotedOps(q)
    import q.tasty
    import q.tasty.{Type => _, _}
    val attrType = typeOf[AttributeEncoder[_]]
    val textType = typeOf[TextEncoder[_]]
    val elementType = typeOf[ElementEncoder[_]]

    val params = FetchGroup.fetchProduct[T]
    val groups = params.groupBy(_.category)

    val encodeAttributes = groups.getOrElse(ParamCategory.attribute, Nil).map: 
      param =>
        val p = param.paramType match
          case TypeRef(TermRef(qual, _), _) => qual

        val attributeEncoder = AppliedType(attrType, List(p))
        searchImplicit(attributeEncoder.tycon)

    val encodeText = groups.getOrElse(ParamCategory.text, Nil).map: 
      param =>
        val p = param.paramType match
          case TypeRef(TermRef(qual, _), _) => qual

        val attributeEncoder = AppliedType(textType, List(p))
        searchImplicit(attributeEncoder.tycon)

    val encodeElements = groups.getOrElse(ParamCategory.element, Nil).map:
      param =>
        val p = param.paramType match
          case TypeRef(TermRef(qual, _), _) => qual

        val attributeEncoder = AppliedType(elementType, List(p))
        searchImplicit(attributeEncoder.tycon)

    '{
       new ElementEncoder[$t]:
        def encodeAsElement(
          a: $t,
          sw: PhobosStreamWriter,
          localName: String,
          namespaceUri: Option[String]
        ): Unit = 
          namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))

        end encodeAsElement
     }
  end deriveImpl

  inline def derive[T]: ElementEncoder[T] = ${deriveImpl[T]}
end EncoderDerivation