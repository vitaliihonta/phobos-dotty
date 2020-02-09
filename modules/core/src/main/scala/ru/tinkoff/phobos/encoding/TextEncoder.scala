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

trait TextEncoder[A] with
  self =>
  def encodeAsText(a: A, sw: XMLStreamWriter2): Unit

  def contramap[B](f: B => A): TextEncoder[B] =
    new TextEncoder[B] with
      def encodeAsText(b: B, sw: XMLStreamWriter2): Unit = self.encodeAsText(f(b), sw)

end TextEncoder

object TextEncoder with
  given encoderContravariant: Contravariant[TextEncoder] =
    new Contravariant[TextEncoder] with
      def contramap[A, B](fa: TextEncoder[A])(f: B => A): TextEncoder[B] = fa.contramap(f)

  /**
    * Instances
    */
  given stringEncoder: TextEncoder[String] =
    new TextEncoder[String] with
      def encodeAsText(a: String, sw: XMLStreamWriter2): Unit = sw.writeRaw(a)

  given unitEncoder: TextEncoder[Unit] =
    new TextEncoder[Unit] with
      def encodeAsText(a: Unit, sw: XMLStreamWriter2): Unit = ()

  given booleanEncoder: TextEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  given javaBooleanEncoder: TextEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  given charEncoder: TextEncoder[Char]                           = stringEncoder.contramap(_.toString)
  given javaCharacterEncoder: TextEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  given floatEncoder: TextEncoder[Float]                         = stringEncoder.contramap(_.toString)
  given javaFloatEncoder: TextEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  given doubleEncoder: TextEncoder[Double]                       = stringEncoder.contramap(_.toString)
  given javaDoubleEncoder: TextEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  given byteEncoder: TextEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  given javaByteEncoder: TextEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  given shortEncoder: TextEncoder[Short]                         = stringEncoder.contramap(_.toString)
  given javaShortEncoder: TextEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  given intEncoder: TextEncoder[Int]                             = stringEncoder.contramap(_.toString)
  given javaIntegerEncoder: TextEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  given longEncoder: TextEncoder[Long]                           = stringEncoder.contramap(_.toString)
  given javaLongEncoder: TextEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  given bigIntEncoder: TextEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  given javaBigIntegerEncoder: TextEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  given bigDecimalEncoder: TextEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  given javaBigDecimalEncoder: TextEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  given UUIDEncoder: TextEncoder[UUID] = stringEncoder.contramap(_.toString)

  given base64Encoder: TextEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  given localDateTimeEncoder: TextEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  given zonedDateTimeEncoder: TextEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  given localDateEncoder: TextEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  given localTimeEncoder: TextEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
end TextEncoder
