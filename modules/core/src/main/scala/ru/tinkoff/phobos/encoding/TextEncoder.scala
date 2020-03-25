package ru.tinkoff.phobos.encoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.Contravariant
import org.codehaus.stax2.XMLStreamWriter2

/**
 * Use XmlEncoder for encoding XML documents.
 *
 * TextEncoder instance must exist for every type encoded to text inside XML element.
 * This typeclass is used for encoding case class parameters with @text annotation.
 *
 * To create new instance use .contramap method of existing instance.
 */

trait TextEncoder[A]:
  self =>
  def encodeAsText(a: A, sw: XMLStreamWriter2): Unit

  def contramap[B](f: B => A): TextEncoder[B] =
    new TextEncoder[B]:
      def encodeAsText(b: B, sw: XMLStreamWriter2): Unit = self.encodeAsText(f(b), sw)

end TextEncoder

object TextEncoder:
  given encoderContravariant as Contravariant[TextEncoder] =
    new Contravariant[TextEncoder]:
      def contramap[A, B](fa: TextEncoder[A])(f: B => A): TextEncoder[B] = fa.contramap(f)

  /**
    * Instances
    */
  given stringEncoder as TextEncoder[String] =
    new TextEncoder[String]:
      def encodeAsText(a: String, sw: XMLStreamWriter2): Unit = sw.writeRaw(a)

  given unitEncoder as TextEncoder[Unit] =
    new TextEncoder[Unit]:
      def encodeAsText(a: Unit, sw: XMLStreamWriter2): Unit = ()

  given booleanEncoder as TextEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  given javaBooleanEncoder as TextEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  given charEncoder as TextEncoder[Char]                           = stringEncoder.contramap(_.toString)
  given javaCharacterEncoder as TextEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  given floatEncoder as TextEncoder[Float]                         = stringEncoder.contramap(_.toString)
  given javaFloatEncoder as TextEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  given doubleEncoder as TextEncoder[Double]                       = stringEncoder.contramap(_.toString)
  given javaDoubleEncoder as TextEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  given byteEncoder as TextEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  given javaByteEncoder as TextEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  given shortEncoder as TextEncoder[Short]                         = stringEncoder.contramap(_.toString)
  given javaShortEncoder as TextEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  given intEncoder as TextEncoder[Int]                             = stringEncoder.contramap(_.toString)
  given javaIntegerEncoder as TextEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  given longEncoder as TextEncoder[Long]                           = stringEncoder.contramap(_.toString)
  given javaLongEncoder as TextEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  given bigIntEncoder as TextEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  given javaBigIntegerEncoder as TextEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  given bigDecimalEncoder as TextEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  given javaBigDecimalEncoder as TextEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  given UUIDEncoder as TextEncoder[UUID] = stringEncoder.contramap(_.toString)

  given base64Encoder as TextEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  given localDateTimeEncoder as TextEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  given zonedDateTimeEncoder as TextEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  given localDateEncoder as TextEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  given localTimeEncoder as TextEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
end TextEncoder
