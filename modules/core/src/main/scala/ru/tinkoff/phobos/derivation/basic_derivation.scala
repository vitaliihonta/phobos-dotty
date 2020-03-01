package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.decoding._
import scala.compiletime._

final case class CaseClassParam[
  T, 
  P <: ParamCategory,
  localName <: String,
  xmlName <: String,
  namespaceUri <: String]()



enum OneOfDecoders[T] with
  case Elem(decoder: ElementDecoder[T])
  case Attr(decoder: AttributeDecoder[T])
  case Text(decoder: TextDecoder[T])
end OneOfDecoders

final case class Param[T](
  decoder: OneOfDecoders[T],
)


inline def deriveProductDecoders[T <: Tuple] <: Tuple = 
  erasedValue[T] match
    case _: Unit => ()
    case _: (CaseClassParam[t, p, localName, xmlName, namespaceUri] *: tail) =>
      val derivedImplicit = erasedValue[p] match
        case _: ParamCategory.element.type =>
          summonFrom { 
            case dec: ElementDecoder[t] => OneOfDecoders.Elem[t](dec)
          }
        case _: ParamCategory.attribute.type =>
          summonFrom { 
            case dec: AttributeDecoder[t] => OneOfDecoders.Attr[t](dec)
          }
        case _: ParamCategory.text.type =>
          summonFrom { 
            case dec: TextDecoder[t] => OneOfDecoders.Text[t](dec)
          }

      Param(derivedImplicit) *: deriveProductCodecs[tail]