package ru.tinkoff.phobos.decoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}
import cats.Functor
import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.TextDecoder.{EMappedDecoder, MappedDecoder}
import scala.annotation.tailrec

/**
 * Warning! This is a complicated internal API which may change in future.
 * Do not implement or use this trait directly unless you know what you are doing.
 *
 * Use XmlDecoder for decoding XML documents.
 *
 * TextDecoder instance must exist for every type decoded from text inside XML element.
 * This typeclass is used for decoding case class parameters with @text annotation.
 *
 * To create new instance use .map or .emap method of existing instance.
 */
trait TextDecoder[A] with 
  self =>
  def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[A]
  def result(history: List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): TextDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): TextDecoder[B] = new EMappedDecoder(self, f)
end TextDecoder

object TextDecoder with

  class MappedDecoder[A, B](fa: TextDecoder[A], f: A => B) extends TextDecoder[B] with

    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[B] =
      MappedDecoder[A, B](fa.decodeAsText(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history).map(f)

    val isCompleted: Boolean = fa.isCompleted

    override def toString: String = s"MappedDecoder(${fa.toString})"
  end MappedDecoder

  final class EMappedDecoder[A, B](fa: TextDecoder[A], f: (List[String], A) => Either[DecodingError, B])
      extends TextDecoder[B] with

    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[B] =
      EMappedDecoder(fa.decodeAsText(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history) match
      case Right(a)    => f(history, a)
      case Left(error) => Left(error)

    def isCompleted: Boolean = fa.isCompleted
  end EMappedDecoder

  given decoderFunctor: Functor[TextDecoder] =
    new Functor[TextDecoder] with
      def map[A, B](fa: TextDecoder[A])(f: A => B): TextDecoder[B] = fa.map(f)

  final class ConstDecoder[A](a: A) extends TextDecoder[A] with
    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Right(a)

    val isCompleted: Boolean = true

    override def toString: String = s"ConstDecoder($a)"
  end ConstDecoder

  final class FailedDecoder[A](decodingError: DecodingError) extends TextDecoder[A] with
    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Left(decodingError)

    val isCompleted: Boolean = true

    override def toString: String = s"FailedDecoder($decodingError)"
  end FailedDecoder

  /**
    * Instances
    */
  class StringDecoder(string: String = "") extends TextDecoder[String] with
    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[String] = 
      val stringBuilder = new StringBuilder(string)
      @tailrec
      def go(): TextDecoder[String] = 
        if c.isCharacters() || c.getEventType() == XMLStreamConstants.CDATA then
          stringBuilder.append(c.getText())
          c.next()
          go()
        else if c.isStartElement() then
          if c.getLocalName() == localName then
            c.next()
            go()
          else 
            StringDecoder(stringBuilder.mkString)
        else if c.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE then
          c.next()
          StringDecoder(stringBuilder.mkString)
        else if c.isEndElement() then
          ConstDecoder(stringBuilder.mkString)
        else
          FailedDecoder(c.error(s"Unexpected event: '${c.getEventType()}'"))
      end go
      go()

    def result(history: List[String]): Either[DecodingError, String] =
      Left(DecodingError("Decoding not complete", history))

    val isCompleted: Boolean = false

    override def toString: String = s"StringDecoder($string)"
  end StringDecoder

  given stringDecoder: TextDecoder[String] = StringDecoder()

  given unitDecoder: TextDecoder[Unit] = ConstDecoder[Unit](())

  given booleanDecoder: TextDecoder[Boolean] =
    stringDecoder.emap:
      (history, string) =>
        string match
          case "true" | "1"  => Right(true)
          case "false" | "0" => Right(false)
          case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))

  given javaBooleanDecoder: TextDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())

  given charDecoder: TextDecoder[Char] =
    stringDecoder.emap: 
      (history, string) =>
        if string.length != 1 then
          Left(DecodingError("Value too long for char", history))
        else
          Right(string.head)

  given javaCharacterDecoder: TextDecoder[java.lang.Character] = charDecoder.map(_.charValue())
  given floatDecoder: TextDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
  given javaFloatDecoder: TextDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
  given doubleDecoder: TextDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
  given javaDoubleDecoder: TextDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
  given byteDecoder: TextDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
  given javaByteDecoder: TextDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
  given shortDecoder: TextDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
  given javaShortDecoder: TextDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
  given intDecoder: TextDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
  given javaIntegerDecoder: TextDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
  given longDecoder: TextDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
  given javaLongDecoder: TextDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
  given bigIntDecoder: TextDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))

  given javaBigIntegerDecoder: TextDecoder[java.math.BigInteger] =
    stringDecoder.emap(wrapException(str => java.math.BigInteger(str)))

  given bigDecimalDecoder: TextDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
  given javaBigDecimalDecoder: TextDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
  given UUIDDecoder: TextDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))

  given base64Decoder: TextDecoder[Array[Byte]] = stringDecoder.emap(wrapException(Base64.getDecoder.decode))

  given localDateTimeDecoder: TextDecoder[LocalDateTime] =
    stringDecoder.emap(wrapException(LocalDateTime.parse))

  given zonedDateTimeDecoder: TextDecoder[ZonedDateTime] =
    stringDecoder.emap(wrapException(ZonedDateTime.parse))

  given localDateDecoder: TextDecoder[LocalDate] =
    stringDecoder.emap(wrapException(LocalDate.parse))

  given localTimeDecoder: TextDecoder[LocalTime] =
    stringDecoder.emap(wrapException(LocalTime.parse))
end TextDecoder