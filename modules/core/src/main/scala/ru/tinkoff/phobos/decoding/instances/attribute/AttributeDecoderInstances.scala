package ru.tinkoff.phobos.decoding.instances.attribute

import ru.tinkoff.phobos.decoding._
import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

trait AttributeDecoderInstances with

  /**
    * Instances
    */
    given stringDecoder: AttributeDecoder[String] =
      new AttributeDecoder[String] with
        def decodeAsAttribute(c: Cursor,
                              localName: String,
                              namespaceUri: Option[String]): Either[DecodingError, String] =
          val idx = c.getAttributeIndex(namespaceUri.orNull, localName)
          if idx > -1 then
            Right(c.getAttributeValue(idx))
          else
            Left(c.error(s"Missing '$localName' attribute"))
    end stringDecoder
  
    given unitDecoder: AttributeDecoder[Unit] = new AttributeDecoder[Unit] with
      def decodeAsAttribute(c: Cursor, localName: String, namespaceUri: Option[String]): Either[DecodingError, Unit] =
        Right(())
  
    given booleanDecoder: AttributeDecoder[Boolean] =
      stringDecoder.emap:
        (history, string) =>
          string match
            case "true" | "1"  => Right(true)
            case "false" | "0" => Right(false)
            case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))
  
    given javaBooleanDecoder: AttributeDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())
  
    given charDecoder: AttributeDecoder[Char] =
      stringDecoder.emap:
        (history, string) =>
          if string.length != 1 then
            Left(DecodingError("Value too long for char", history))
          else
            Right(string.head)
    
    given javaCharacterDecoder: AttributeDecoder[java.lang.Character] = charDecoder.map(_.charValue())
    given floatDecoder: AttributeDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
    given javaFloatDecoder: AttributeDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
    given doubleDecoder: AttributeDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
    given javaDoubleDecoder: AttributeDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
    given byteDecoder: AttributeDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
    given javaByteDecoder: AttributeDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
    given shortDecoder: AttributeDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
    given javaShortDecoder: AttributeDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
    given intDecoder: AttributeDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
    given javaIntegerDecoder: AttributeDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
    given longDecoder: AttributeDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
    given javaLongDecoder: AttributeDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
    given bigIntDecoder: AttributeDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))
  
    given javaBigIntegerDecoder: AttributeDecoder[java.math.BigInteger] =
      stringDecoder.emap(wrapException(str => java.math.BigInteger(str)))
  
    given bigDecimalDecoder: AttributeDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
    given javaBigDecimalDecoder: AttributeDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
    given UUIDDecoder: AttributeDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))
  
    given base64Decoder: AttributeDecoder[Array[Byte]] =
      stringDecoder.emap(wrapException(Base64.getDecoder.decode))
  
    given optionDecoder[A](using decoder: AttributeDecoder[A]) as AttributeDecoder[Option[A]] =
      new AttributeDecoder[Option[A]] with
        def decodeAsAttribute(c: Cursor,
                              localName: String,
                              namespaceUri: Option[String]): Either[DecodingError, Option[A]] =
          val idx = c.getAttributeIndex(namespaceUri.orNull, localName)
          if idx > -1 then
            decoder.decodeAsAttribute(c, localName, namespaceUri).map(Some.apply)
          else
            Right(None)
    end optionDecoder
  
    given localDateTimeDecoder: AttributeDecoder[LocalDateTime] =
      stringDecoder.emap(wrapException(LocalDateTime.parse))
  
    given zonedDateTimeDecoder: AttributeDecoder[ZonedDateTime] =
      stringDecoder.emap(wrapException(ZonedDateTime.parse))
  
    given localDateDecoder: AttributeDecoder[LocalDate] =
      stringDecoder.emap(wrapException(LocalDate.parse))
  
    given localTimeDecoder: AttributeDecoder[LocalTime] =
      stringDecoder.emap(wrapException(LocalTime.parse))

end AttributeDecoderInstances