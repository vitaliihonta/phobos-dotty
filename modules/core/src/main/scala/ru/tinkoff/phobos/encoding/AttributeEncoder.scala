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
trait AttributeEncoder[A]: 
  self =>
  def encodeAsAttribute(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): AttributeEncoder[B] =
    new AttributeEncoder[B]:
      def encodeAsAttribute(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsAttribute(f(b), sw, localName, namespaceUri)

end AttributeEncoder

object AttributeEncoder:
  given encoderContravariant as Contravariant[AttributeEncoder] =
    new Contravariant[AttributeEncoder]:
      def contramap[A, B](fa: AttributeEncoder[A])(f: B => A): AttributeEncoder[B] = fa.contramap(f)
    
  /**
    * Instances
    */
  given stringEncoder as AttributeEncoder[String] =
    new AttributeEncoder[String]:
      def encodeAsAttribute(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        namespaceUri.fold(sw.writeAttribute(localName, a))(ns => sw.writeAttribute(ns, localName, a))

  given unitEncoder as AttributeEncoder[Unit] =
    new AttributeEncoder[Unit]:
      def encodeAsAttribute(a: Unit, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = ()

  given booleanEncoder as AttributeEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  given javaBooleanEncoder as AttributeEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  given charEncoder as AttributeEncoder[Char]                           = stringEncoder.contramap(_.toString)
  given javaCharacterEncoder as AttributeEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  given floatEncoder as AttributeEncoder[Float]                         = stringEncoder.contramap(_.toString)
  given javaFloatEncoder as AttributeEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  given doubleEncoder as AttributeEncoder[Double]                       = stringEncoder.contramap(_.toString)
  given javaDoubleEncoder as AttributeEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  given byteEncoder as AttributeEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  given javaByteEncoder as AttributeEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  given shortEncoder as AttributeEncoder[Short]                         = stringEncoder.contramap(_.toString)
  given javaShortEncoder as AttributeEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  given intEncoder as AttributeEncoder[Int]                             = stringEncoder.contramap(_.toString)
  given javaIntegerEncoder as AttributeEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  given longEncoder as AttributeEncoder[Long]                           = stringEncoder.contramap(_.toString)
  given javaLongEncoder as AttributeEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  given bigIntEncoder as AttributeEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  given javaBigIntegerEncoder as AttributeEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  given bigDecimalEncoder as AttributeEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  given javaBigDecimalEncoder as AttributeEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  given UUIDEncoder as AttributeEncoder[UUID] = stringEncoder.contramap(_.toString)

  given base64Encoder as AttributeEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  given optionEncoder[A](using encoder: AttributeEncoder[A]) as AttributeEncoder[Option[A]] =
    new AttributeEncoder[Option[A]]:
      def encodeAsAttribute(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsAttribute(_, sw, localName, namespaceUri))

  given localDateTimeEncoder as AttributeEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  given zonedDateTimeEncoder as AttributeEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  given localDateEncoder as AttributeEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  given localTimeEncoder as AttributeEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
end AttributeEncoder
