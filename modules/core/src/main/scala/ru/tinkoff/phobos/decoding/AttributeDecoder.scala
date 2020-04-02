package ru.tinkoff.phobos.decoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}
import cats.Functor
import com.fasterxml.aalto._
import ru.tinkoff.phobos.decoding.instances.attribute.AttributeDecoderInstances

/**
 * Warning! This is an internal API which may change in future.
 * Do not implement or use this trait directly unless you know what you are doing.
 *
 * Use XmlDecoder for decoding XML documents.
 *new 
 * AttributeDecoder instance must exist for every type decoded from attribute.
 * This typeclass is used for decoding case class parameters with @attr annotation.
 *
 * To create new instance use .map or .emap method of existing instance.
 */
trait AttributeDecoder[A]:
  self =>
  def decodeAsAttribute(c: Cursor, localName: String): Either[DecodingError, A]

  def map[B](f: A => B): AttributeDecoder[B] =
    new AttributeDecoder[B]:
      def decodeAsAttribute(c: Cursor, localName: String): Either[DecodingError, B] =
        self.decodeAsAttribute(c, localName).map(f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): AttributeDecoder[B] =
    new AttributeDecoder[B]:
      def decodeAsAttribute(c: Cursor, localName: String): Either[DecodingError, B] =
        self.decodeAsAttribute(c, localName) match
          case Right(a)    => f(c.history, a)
          case Left(error) => Left(error)
end AttributeDecoder

object AttributeDecoder extends AttributeDecoderInstances:
  given attributeDecoderFunctor as Functor[AttributeDecoder] =
    new Functor[AttributeDecoder]:
      def map[A, B](fa: AttributeDecoder[A])(f: A => B): AttributeDecoder[B] = fa.map(f)

end AttributeDecoder
