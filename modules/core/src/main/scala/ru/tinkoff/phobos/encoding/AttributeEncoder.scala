package ru.tinkoff.phobos.encoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.Contravariant
import org.codehaus.stax2.XMLStreamWriter2

/**
 * Warning! This is an internal API which may change in future.
 * Do not implement or use this trait directly unless you know what you are doing.
 *
 * Use XmlEncoder for encoding XML documents.
 *
 * AttributeEncoder instance must exist for every type encoded to attribute.
 * This typeclass is used for encoding case class parameters with @attr annotation.
 *
 * To create new instance use .contramap method of existing instance.
 */
trait AttributeEncoder[A] with 
  self =>
  def encodeAsAttribute(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): AttributeEncoder[B] =
    new AttributeEncoder[B] with
      def encodeAsAttribute(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsAttribute(f(b), sw, localName, namespaceUri)

end AttributeEncoder

object AttributeEncoder with
  given encoderContravariant: Contravariant[AttributeEncoder] =
    new Contravariant[AttributeEncoder] with
      def contramap[A, B](fa: AttributeEncoder[A])(f: B => A): AttributeEncoder[B] = fa.contramap(f)
    
  /**
    * Instances
    */
  given stringEncoder: AttributeEncoder[String] =
    new AttributeEncoder[String] with
      def encodeAsAttribute(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        namespaceUri.fold(sw.writeAttribute(localName, a))(ns => sw.writeAttribute(ns, localName, a))

  given unitEncoder: AttributeEncoder[Unit] =
    new AttributeEncoder[Unit] with
      def encodeAsAttribute(a: Unit, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = ()

  given booleanEncoder: AttributeEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  given javaBooleanEncoder: AttributeEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  given charEncoder: AttributeEncoder[Char]                           = stringEncoder.contramap(_.toString)
  given javaCharacterEncoder: AttributeEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  given floatEncoder: AttributeEncoder[Float]                         = stringEncoder.contramap(_.toString)
  given javaFloatEncoder: AttributeEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  given doubleEncoder: AttributeEncoder[Double]                       = stringEncoder.contramap(_.toString)
  given javaDoubleEncoder: AttributeEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  given byteEncoder: AttributeEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  given javaByteEncoder: AttributeEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  given shortEncoder: AttributeEncoder[Short]                         = stringEncoder.contramap(_.toString)
  given javaShortEncoder: AttributeEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  given intEncoder: AttributeEncoder[Int]                             = stringEncoder.contramap(_.toString)
  given javaIntegerEncoder: AttributeEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  given longEncoder: AttributeEncoder[Long]                           = stringEncoder.contramap(_.toString)
  given javaLongEncoder: AttributeEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  given bigIntEncoder: AttributeEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  given javaBigIntegerEncoder: AttributeEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  given bigDecimalEncoder: AttributeEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  given javaBigDecimalEncoder: AttributeEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  given UUIDEncoder: AttributeEncoder[UUID] = stringEncoder.contramap(_.toString)

  given base64Encoder: AttributeEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  given optionEncoder[A](using encoder: AttributeEncoder[A]) as AttributeEncoder[Option[A]] =
    new AttributeEncoder[Option[A]] with
      def encodeAsAttribute(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsAttribute(_, sw, localName, namespaceUri))

  given localDateTimeEncoder: AttributeEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  given zonedDateTimeEncoder: AttributeEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  given localDateEncoder: AttributeEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  given localTimeEncoder: AttributeEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
end AttributeEncoder
