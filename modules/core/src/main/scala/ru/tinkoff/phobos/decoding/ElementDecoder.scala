package ru.tinkoff.phobos.decoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.Functor
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.ElementDecoder.{EMappedDecoder, MappedDecoder}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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
trait ElementDecoder[A] with
  self =>
  def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[A]
  def result(history: List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): ElementDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): ElementDecoder[B] = new EMappedDecoder(self, f)

end ElementDecoder

object ElementDecoder with

  def errorIfWrongName[A](c: Cursor, localName: String, namespaceUri: Option[String]): Option[FailedDecoder[A]] = 
    namespaceUri match 
      case _ if c.getLocalName() != localName =>
        Some(new FailedDecoder(c.error(s"Invalid local name. Expected '$localName', but found '${c.getLocalName()}'")))
      case Some(uri) if uri != c.getNamespaceURI() =>
        Some(new FailedDecoder(c.error(s"Invalid namespace. Expected '$uri', but found '${c.getNamespaceURI()}'")))
      case None if c.getNamespaceURI() != "" =>
        Some(new FailedDecoder(c.error(s"Invalid namespace. Expected no namespace, but found '${c.getNamespaceURI()}'")))
      case _ => None

  def isNil(c: Cursor): Boolean = 
    val nilIdx = c.getAttributeIndex("http://www.w3.org/2001/XMLSchema-instance", "nil")
    nilIdx > -1 && c.getAttributeValue(nilIdx) == "true"

  final class MappedDecoder[A, B](fa: ElementDecoder[A], f: A => B) extends ElementDecoder[B] with
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[B] =
      new MappedDecoder(fa.decodeAsElement(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history).map(f)

    val isCompleted: Boolean = fa.isCompleted

    override def toString: String = s"MappedDecoder(${fa.toString})"
  end MappedDecoder

  final class EMappedDecoder[A, B](fa: ElementDecoder[A], f: (List[String], A) => Either[DecodingError, B])
      extends ElementDecoder[B] with
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[B] =
      new EMappedDecoder(fa.decodeAsElement(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] =
      fa.result(history) match {
        case Right(a)    => f(history, a)
        case Left(error) => Left(error)
      }

    def isCompleted: Boolean = fa.isCompleted
  end EMappedDecoder

  given decoderFunctor: Functor[ElementDecoder] =
    new Functor[ElementDecoder] with
      def map[A, B](fa: ElementDecoder[A])(f: A => B): ElementDecoder[B] = fa.map(f)

  final class ConstDecoder[A](a: A) extends ElementDecoder[A] with
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[A] =
      new FailedDecoder[A](c.error("Element is already decoded (Most likely it occurred more than once)"))

    def result(history: List[String]): Either[DecodingError, A] = Right(a)

    val isCompleted: Boolean = true

    override def toString: String = s"ConstDecoder($a)"
  end ConstDecoder

  final class FailedDecoder[A](decodingError: DecodingError) extends ElementDecoder[A] with
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Left(decodingError)

    val isCompleted: Boolean = true

    override def toString: String = s"FailedDecoder($decodingError)"
  end FailedDecoder

  /**
    * Instances
    */
  final class StringDecoder(string: String = "") extends ElementDecoder[String] with
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[String] = 
      val stringBuilder = new StringBuilder(string)

      @tailrec
      def go(): ElementDecoder[String] = 
        if c.isCharacters() || c.getEventType() == XMLStreamConstants.CDATA then
          stringBuilder.append(c.getText())
          c.next()
          go()
        else if c.isEndElement() then
          ElementDecoder.errorIfWrongName[String](c, localName, namespaceUri).getOrElse:
            c.next()
            ConstDecoder(stringBuilder.mkString)
        else if (c.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE) then
          c.next()
          StringDecoder(stringBuilder.mkString)
        else
          FailedDecoder(c.error(s"Unexpected event: '${c.getEventType()}'"))
      end go

      if c.isStartElement() && stringBuilder.isEmpty then
        ElementDecoder.errorIfWrongName[String](c, localName, namespaceUri).getOrElse:
          c.next()
          go()
      else go()

    def result(history: List[String]): Either[DecodingError, String] =
      Left(DecodingError("Decoding not complete", history))

    val isCompleted: Boolean = false

    override def toString: String =
      s"StringDecoder($string)"
  end StringDecoder

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
          ElementDecoder.errorIfWrongName[Option[A]](c, localName, namespaceUri).getOrElse:
            if ElementDecoder.isNil(c) || c.isEmptyElement() then
              c.next()
              ConstDecoder(None)
            else
              decoder.map[Option[A]](a => Some(a)).decodeAsElement(c, localName, namespaceUri)
        else 
          FailedDecoder[Option[A]](c.error(s"Unexpected event: '${c.getEventType()}'"))
        

      def result(history: List[String]): Either[DecodingError, Option[A]] = Right(None)

      val isCompleted: Boolean = true

  end optionDecoder

  class ListDecoder[A](list: List[A] = Nil, currentItemDecoderOpt: Option[ElementDecoder[A]] = None)(
      given itemDecoder: ElementDecoder[A])
      extends ElementDecoder[List[A]] with
    def decodeAsElement(cursor: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[List[A]] =
      if cursor.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE then
        this
      else
        val listBuffer: ListBuffer[A] = ListBuffer.empty
        listBuffer.appendAll(list)

        @tailrec
        def go(currentItemDecoder: Option[ElementDecoder[A]]): ElementDecoder[List[A]] = 
          val decoder = currentItemDecoder.getOrElse(itemDecoder)

          if currentItemDecoder.isDefined || (cursor.isStartElement() && cursor.getLocalName() == localName) then
            if currentItemDecoder.isEmpty && ElementDecoder.isNil(cursor) then
              cursor.next()
              go(None)
            else
              val newDecoder = decoder.decodeAsElement(cursor, localName, namespaceUri)
              if newDecoder.isCompleted then
                newDecoder.result(cursor.history) match
                  case Right(a) =>
                    listBuffer.append(a)
                    go(None)
                  case Left(err) =>
                    FailedDecoder(err)
              else
                ListDecoder[A](listBuffer.toList, Some(newDecoder))
          else if cursor.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE || cursor.isStartElement() || cursor.isEndElement() then
            ListDecoder[A](listBuffer.toList)
          else
            cursor.next()
            go(None)
        end go
        go(currentItemDecoderOpt)
    end decodeAsElement

    def result(history: List[String]): Either[DecodingError, List[A]] =
      if currentItemDecoderOpt.isEmpty then
        Right(list)
      else 
        Left(DecodingError("Decoding not complete", history))

    def isCompleted: Boolean = currentItemDecoderOpt.isEmpty

    override def toString: String =
      s"ListDecoder(${itemDecoder.toString})"
  end ListDecoder

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
end ElementDecoder
