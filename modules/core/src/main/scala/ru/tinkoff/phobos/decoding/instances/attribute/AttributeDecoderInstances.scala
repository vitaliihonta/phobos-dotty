package ru.tinkoff.phobos.decoding.instances.attribute

import ru.tinkoff.phobos.decoding._
import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

trait AttributeDecoderInstances:

  /**
    * Instances
    */
    given stringDecoder as AttributeDecoder[String] =
      new AttributeDecoder[String]:
        def decodeAsAttribute(c: Cursor,
                              localName: String): Either[DecodingError, String] =
          val idx = c.getAttributeIndex(null, localName)
          if idx > -1 then
            Right(c.getAttributeValue(idx))
          else
            Left(c.error(s"Missing '$localName' attribute"))
    end stringDecoder
  
    given unitDecoder as AttributeDecoder[Unit] = new AttributeDecoder[Unit]:
      def decodeAsAttribute(c: Cursor, localName: String): Either[DecodingError, Unit] =
        Right(())
  
    given booleanDecoder as AttributeDecoder[Boolean] =
      stringDecoder.emap:
        (history, string) =>
          string match
            case "true" | "1"  => Right(true)
            case "false" | "0" => Right(false)
            case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))
  
    given javaBooleanDecoder as AttributeDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())
  
    given charDecoder as AttributeDecoder[Char] =
      stringDecoder.emap:
        (history, string) =>
          if string.length != 1 then
            Left(DecodingError("Value too long for char", history))
          else
            Right(string.head)
    
    given javaCharacterDecoder as AttributeDecoder[java.lang.Character] = charDecoder.map(_.charValue())
    given floatDecoder as AttributeDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
    given javaFloatDecoder as AttributeDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
    given doubleDecoder as AttributeDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
    given javaDoubleDecoder as AttributeDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
    given byteDecoder as AttributeDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
    given javaByteDecoder as AttributeDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
    given shortDecoder as AttributeDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
    given javaShortDecoder as AttributeDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
    given intDecoder as AttributeDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
    given javaIntegerDecoder as AttributeDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
    given longDecoder as AttributeDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
    given javaLongDecoder as AttributeDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
    given bigIntDecoder as AttributeDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))
  
    given javaBigIntegerDecoder as AttributeDecoder[java.math.BigInteger] =
      stringDecoder.emap(wrapException(str => java.math.BigInteger(str)))
  
    given bigDecimalDecoder as AttributeDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
    given javaBigDecimalDecoder as AttributeDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
    given UUIDDecoder as AttributeDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))
  
    given base64Decoder as AttributeDecoder[Array[Byte]] =
      stringDecoder.emap(wrapException(Base64.getDecoder.decode))
  
    given optionDecoder[A](using decoder: AttributeDecoder[A]) as AttributeDecoder[Option[A]] =
      new AttributeDecoder[Option[A]]:
        def decodeAsAttribute(c: Cursor,
                              localName: String): Either[DecodingError, Option[A]] =
          val idx = c.getAttributeIndex(null, localName)
          if idx > -1 then
            decoder.decodeAsAttribute(c, localName).map(Some.apply)
          else
            Right(None)
    end optionDecoder
  
    given localDateTimeDecoder as AttributeDecoder[LocalDateTime] =
      stringDecoder.emap(wrapException(LocalDateTime.parse))
  
    given zonedDateTimeDecoder as AttributeDecoder[ZonedDateTime] =
      stringDecoder.emap(wrapException(ZonedDateTime.parse))
  
    given localDateDecoder as AttributeDecoder[LocalDate] =
      stringDecoder.emap(wrapException(LocalDate.parse))
  
    given localTimeDecoder as AttributeDecoder[LocalTime] =
      stringDecoder.emap(wrapException(LocalTime.parse))

end AttributeDecoderInstances