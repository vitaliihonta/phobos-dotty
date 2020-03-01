package ru.tinkoff.phobos.decoding.instances.element

import ru.tinkoff.phobos.decoding._
import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}

trait ElementDecoderInstances with

  given stringDecoder: ElementDecoder[String] = StringDecoder()

  given unitDecoder: ElementDecoder[Unit] = ConstDecoder[Unit](())

  given booleanDecoder: ElementDecoder[Boolean] =
    stringDecoder.emap:
      (history, string) =>
        string match
          case "true" | "1"  => Right(true)
          case "false" | "0" => Right(false)
          case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))

  given javaBooleanDecoder: ElementDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())

  given charDecoder: ElementDecoder[Char] =
    stringDecoder.emap:
      (history, string) =>
        if string.length != 1 then
          Left(DecodingError("Value too long for char", history))
        else 
          Right(string.head)
      
  given javaCharacterDecoder: ElementDecoder[java.lang.Character] = charDecoder.map(_.charValue())
  given floatDecoder: ElementDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
  given javaFloatDecoder: ElementDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
  given doubleDecoder: ElementDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
  given javaDoubleDecoder: ElementDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
  given byteDecoder: ElementDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
  given javaByteDecoder: ElementDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
  given shortDecoder: ElementDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
  given javaShortDecoder: ElementDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
  given intDecoder: ElementDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
  given javaIntegerDecoder: ElementDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
  given longDecoder: ElementDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
  given javaLongDecoder: ElementDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
  given bigIntDecoder: ElementDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))

  given javaBigIntegerDecoder: ElementDecoder[java.math.BigInteger] =
    stringDecoder.emap(wrapException(str => java.math.BigInteger(str)))

  given bigDecimalDecoder: ElementDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
  given javaBigDecimalDecoder: ElementDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
  given UUIDDecoder: ElementDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))

  given base64Decoder: ElementDecoder[Array[Byte]] = stringDecoder.emap(wrapException(Base64.getDecoder.decode))

  given optionDecoder[A](using decoder: ElementDecoder[A]) as ElementDecoder[Option[A]] =
    new ElementDecoder[Option[A]] with
      def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[Option[A]] =
        if c.isStartElement() then
          errorIfWrongName[Option[A]](c, localName, namespaceUri).getOrElse:
            if isNil(c) || c.isEmptyElement() then
              c.next()
              ConstDecoder(None)
            else
              decoder.map[Option[A]](a => Some(a)).decodeAsElement(c, localName, namespaceUri)
        else 
          FailedDecoder[Option[A]](c.error(s"Unexpected event: '${c.getEventType()}'"))
        

      def result(history: List[String]): Either[DecodingError, Option[A]] = Right(None)

      val isCompleted: Boolean = true

  end optionDecoder

  given listDecoder[A](using decoder: ElementDecoder[A]) as ElementDecoder[List[A]] = ListDecoder[A]()

  given seqDecoder[A: ElementDecoder]: ElementDecoder[Seq[A]] =
    listDecoder[A].map(_.toSeq)

  given setDecoder[A: ElementDecoder]: ElementDecoder[Set[A]] =
    listDecoder[A].map(_.toSet)

  given vectorDecoder[A: ElementDecoder]: ElementDecoder[Vector[A]] =
    listDecoder[A].map(_.toVector)

  given chainDecoder[A: ElementDecoder]: ElementDecoder[Chain[A]] =
    listDecoder[A].map(Chain.fromSeq)

  given nonEmptyListDecoder[A: ElementDecoder]: ElementDecoder[NonEmptyList[A]] =
    listDecoder[A].emap:
      (history, list) =>
        NonEmptyList
          .fromList(list)
          .fold[Either[DecodingError, NonEmptyList[A]]](Left(DecodingError("List is empty", history)))(Right.apply)

  given nonEmptyVectorDecoder[A: ElementDecoder]: ElementDecoder[NonEmptyVector[A]] =
    vectorDecoder[A].emap:
      (history, vector) =>
        NonEmptyVector
          .fromVector(vector)
          .fold[Either[DecodingError, NonEmptyVector[A]]](Left(DecodingError("Vector is empty", history)))(Right.apply)

  given nonEmptyChainDecoder[A: ElementDecoder]: ElementDecoder[NonEmptyChain[A]] =
    chainDecoder[A].emap:
      (history, chain) =>
        NonEmptyChain
          .fromChain(chain)
          .fold[Either[DecodingError, NonEmptyChain[A]]](Left(DecodingError("Chain is empty", history)))(Right.apply)

  given localDateTimeDecoder: ElementDecoder[LocalDateTime] =
    stringDecoder.emap(wrapException(LocalDateTime.parse))

  given zonedDateTimeDecoder: ElementDecoder[ZonedDateTime] =
    stringDecoder.emap(wrapException(ZonedDateTime.parse))

  given localDateDecoder: ElementDecoder[LocalDate] =
    stringDecoder.emap(wrapException(LocalDate.parse))

  given localTimeDecoder: ElementDecoder[LocalTime] =
    stringDecoder.emap(wrapException(LocalTime.parse))