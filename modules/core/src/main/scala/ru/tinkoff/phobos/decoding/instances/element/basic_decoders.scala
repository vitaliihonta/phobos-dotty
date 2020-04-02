package ru.tinkoff.phobos.decoding.instances.element

import ru.tinkoff.phobos.decoding._
import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}
import scala.annotation.tailrec
import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import scala.collection.mutable.ListBuffer

/**
  * Instances
  */
final class StringDecoder(string: String = "") extends ElementDecoder[String]:
  def decodeAsElement(c: Cursor, localName: String): ElementDecoder[String] = 
    val stringBuilder = new StringBuilder(string)

    @tailrec
    def go(): ElementDecoder[String] = 
      if c.isCharacters() || c.getEventType() == XMLStreamConstants.CDATA then
        stringBuilder.append(c.getText())
        c.next()
        go()
      else if c.isEndElement() then
        errorIfWrongName[String](c, localName).getOrElse:
          c.next()
          ConstDecoder(stringBuilder.mkString)
      else if (c.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE) then
        c.next()
        StringDecoder(stringBuilder.mkString)
      else
        FailedDecoder(c.error(s"Unexpected event: '${c.getEventType()}'"))
    end go

    if c.isStartElement() && stringBuilder.isEmpty then
      errorIfWrongName[String](c, localName).getOrElse:
        c.next()
        go()
    else go()

  def result(history: List[String]): Either[DecodingError, String] =
    Left(DecodingError("Decoding not complete", history))

  val isCompleted: Boolean = false

  override def toString: String =
    s"StringDecoder($string)"
end StringDecoder

final class ConstDecoder[A](a: A) extends ElementDecoder[A]:
  def decodeAsElement(c: Cursor, localName: String): ElementDecoder[A] =
    new FailedDecoder[A](c.error("Element is already decoded (Most likely it occurred more than once)"))

  def result(history: List[String]): Either[DecodingError, A] = Right(a)

  val isCompleted: Boolean = true

  override def toString: String = s"ConstDecoder($a)"
end ConstDecoder

final class FailedDecoder[A](decodingError: DecodingError) extends ElementDecoder[A]:
  def decodeAsElement(c: Cursor, localName: String): ElementDecoder[A] = this

  def result(history: List[String]): Either[DecodingError, A] = Left(decodingError)

  val isCompleted: Boolean = true

  override def toString: String = s"FailedDecoder($decodingError)"
end FailedDecoder


def errorIfWrongName[A](c: Cursor, localName: String): Option[FailedDecoder[A]] = 
  if c.getLocalName() != localName then
    Some(new FailedDecoder(c.error(s"Invalid local name. Expected '$localName', but found '${c.getLocalName()}'")))
  else None

def isNil(c: Cursor): Boolean = 
  val nilIdx = c.getAttributeIndex("http://www.w3.org/2001/XMLSchema-instance", "nil")
  nilIdx > -1 && c.getAttributeValue(nilIdx) == "true"

final class MappedDecoder[A, B](fa: ElementDecoder[A], f: A => B) extends ElementDecoder[B]:
  def decodeAsElement(c: Cursor, localName: String): ElementDecoder[B] =
    new MappedDecoder(fa.decodeAsElement(c, localName), f)

  def result(history: List[String]): Either[DecodingError, B] = fa.result(history).map(f)

  val isCompleted: Boolean = fa.isCompleted

  override def toString: String = s"MappedDecoder(${fa.toString})"
end MappedDecoder

final class EMappedDecoder[A, B](fa: ElementDecoder[A], f: (List[String], A) => Either[DecodingError, B])
    extends ElementDecoder[B]:
  def decodeAsElement(c: Cursor, localName: String): ElementDecoder[B] =
    new EMappedDecoder(fa.decodeAsElement(c, localName), f)

  def result(history: List[String]): Either[DecodingError, B] =
    fa.result(history) match {
      case Right(a)    => f(history, a)
      case Left(error) => Left(error)
    }

  def isCompleted: Boolean = fa.isCompleted
end EMappedDecoder

class ListDecoder[A](list: List[A] = Nil, currentItemDecoderOpt: Option[ElementDecoder[A]] = None)(
    using itemDecoder: ElementDecoder[A])
    extends ElementDecoder[List[A]]:
  def decodeAsElement(cursor: Cursor, localName: String): ElementDecoder[List[A]] =
    if cursor.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE then
      this
    else
      val listBuffer: ListBuffer[A] = ListBuffer.empty
      listBuffer.appendAll(list)

      @tailrec
      def go(currentItemDecoder: Option[ElementDecoder[A]]): ElementDecoder[List[A]] = 
        val decoder = currentItemDecoder.getOrElse(itemDecoder)

        if currentItemDecoder.isDefined || (cursor.isStartElement() && cursor.getLocalName() == localName) then
          if currentItemDecoder.isEmpty && isNil(cursor) then
            cursor.next()
            go(None)
          else
            val newDecoder = decoder.decodeAsElement(cursor, localName)
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