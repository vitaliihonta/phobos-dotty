package ru.tinkoff.phobos.encoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.{Contravariant, Foldable}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import org.codehaus.stax2.XMLStreamWriter2

/**
 * Warning! This is an internal API which may change in future.
 * Do not implement or use this trait directly unless you know what you are doing.
 *
 * Use XmlEncoder for encoding.
 *
 * ElementEncoder instance must exist for every type encoded to XML element.
 *
 * ElementEncoder instance can be created
 *  - from existing instance by using .contramap (mostly used for "simple" types);
 *  - by macros from ru.tinkoff.phobos.derivation.semiauto package (for case classes and sealed traits).
 *
 *
 * This typeclass describes process of encoding some A value to XML document. Name of the element is
 * not defined in typeclass, it should be passed in encodeAsElement method.
 */
trait ElementEncoder[A] with
  self =>
  def encodeAsElement(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): ElementEncoder[B] =
    new ElementEncoder[B] with
      def encodeAsElement(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsElement(f(b), sw, localName, namespaceUri)

end ElementEncoder

object ElementEncoder with
  given encoderContravariant: Contravariant[ElementEncoder] =
    new Contravariant[ElementEncoder] with
      def contramap[A, B](fa: ElementEncoder[A])(f: B => A): ElementEncoder[B] = fa.contramap(f)

  /**
    * Instances
    */
  given stringEncoder: ElementEncoder[String] =
    new ElementEncoder[String] with
      def encodeAsElement(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))
        sw.writeCharacters(a)
        sw.writeEndElement()
  end stringEncoder

  given unitEncoder: ElementEncoder[Unit] =
    new ElementEncoder[Unit] with
      def encodeAsElement(a: Unit, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = ()

  given booleanEncoder: ElementEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  given javaBooleanEncoder: ElementEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  given charEncoder: ElementEncoder[Char]                           = stringEncoder.contramap(_.toString)
  given javaCharacterEncoder: ElementEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  given floatEncoder: ElementEncoder[Float]                         = stringEncoder.contramap(_.toString)
  given javaFloatEncoder: ElementEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  given doubleEncoder: ElementEncoder[Double]                       = stringEncoder.contramap(_.toString)
  given javaDoubleEncoder: ElementEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  given byteEncoder: ElementEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  given javaByteEncoder: ElementEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  given shortEncoder: ElementEncoder[Short]                         = stringEncoder.contramap(_.toString)
  given javaShortEncoder: ElementEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  given intEncoder: ElementEncoder[Int]                             = stringEncoder.contramap(_.toString)
  given javaIntegerEncoder: ElementEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  given longEncoder: ElementEncoder[Long]                           = stringEncoder.contramap(_.toString)
  given javaLongEncoder: ElementEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  given bigIntEncoder: ElementEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  given javaBigIntegerEncoder: ElementEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  given bigDecimalEncoder: ElementEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  given javaBigDecimalEncoder: ElementEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  given UUIDEncoder: ElementEncoder[UUID] = stringEncoder.contramap(_.toString)

  given base64Encoder: ElementEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  given optionEncoder[A](using encoder: ElementEncoder[A]) as ElementEncoder[Option[A]] =
    new ElementEncoder[Option[A]] with
      def encodeAsElement(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsElement(_, sw, localName, namespaceUri))

  given foldableEncoder[F[_]: Foldable, A](using encoder: ElementEncoder[A]) as ElementEncoder[F[A]] =
    new ElementEncoder[F[A]] with
      def encodeAsElement(vs: F[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        Foldable[F].foldLeft(vs, ())((_, a) => encoder.encodeAsElement(a, sw, localName, namespaceUri))

  given iteratorEncoder[A](using encoder: ElementEncoder[A]) as ElementEncoder[Iterator[A]] =
    new ElementEncoder[Iterator[A]] with
      def encodeAsElement(vs: Iterator[A],
                          sw: XMLStreamWriter2,
                          localName: String,
                          namespaceUri: Option[String]): Unit =
        vs.foreach(a => encoder.encodeAsElement(a, sw, localName, namespaceUri))

  given seqEncoder[A: ElementEncoder]: ElementEncoder[Seq[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given setEncoder[A: ElementEncoder]: ElementEncoder[Set[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given listEncoder[A: ElementEncoder]: ElementEncoder[List[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given vectorEncoder[A: ElementEncoder]: ElementEncoder[Vector[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given chainEncoder[A: ElementEncoder]: ElementEncoder[Chain[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given nonEmptyListEncoder[A: ElementEncoder]: ElementEncoder[NonEmptyList[A]] =
    listEncoder[A].contramap(_.toList)

  given nonEmptyVectorEncoder[A: ElementEncoder]: ElementEncoder[NonEmptyVector[A]] =
    vectorEncoder[A].contramap(_.toVector)

  given nonEmptySetEncoder[A: ElementEncoder]: ElementEncoder[NonEmptySet[A]] =
    setEncoder[A].contramap(_.toSortedSet)

  given nonEmptyChainEncoder[A: ElementEncoder]: ElementEncoder[NonEmptyChain[A]] =
    chainEncoder[A].contramap(_.toChain)

  given localDateTimeEncoder: ElementEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  given zonedDateTimeEncoder: ElementEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  given localDateEncoder: ElementEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  given localTimeEncoder: ElementEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
end ElementEncoder
