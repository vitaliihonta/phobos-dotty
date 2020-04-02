package ru.tinkoff.phobos.decoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}
import cats.Functor
import ru.tinkoff.phobos.decoding.instances.element._

/**
  * Warning! This is a complicated internal API which may change in future.
  * Do not implement or use this trait directly unless you know what you are doing.
  *
  * Use XmlDecoder for decoding XML documents.
  *
  * ElementDecoder instance must exist for every type decoded from XML element.
  *
  * ElementDecoder instance can be created
  *  - from existing instance by using .map or .emap method (mostly used for "simple" types);
  *  - by macros from ru.tinkoff.phobos.derivation.semiauto package (for case classes and sealed traits).
  *
  *
  * This typeclass describes process of decoding some element to an A value. Name of the element is
  * not defined in typeclass, it should be passed in decodeAsElement method.
  */
trait ElementDecoder[A]:
  self =>
  def decodeAsElement(c: Cursor, localName: String): ElementDecoder[A]
  def result(history: List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): ElementDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): ElementDecoder[B] = new EMappedDecoder(self, f)

end ElementDecoder

object ElementDecoder extends ElementDecoderInstances:
  given decoderFunctor as Functor[ElementDecoder] =
    new Functor[ElementDecoder]:
      def map[A, B](fa: ElementDecoder[A])(f: A => B): ElementDecoder[B] = fa.map(f)

end ElementDecoder
