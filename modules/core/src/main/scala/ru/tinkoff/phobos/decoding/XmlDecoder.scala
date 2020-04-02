package ru.tinkoff.phobos.decoding

import cats.Foldable
import javax.xml.stream.XMLStreamConstants
import cats.syntax.option._
import com.fasterxml.aalto.AsyncByteArrayFeeder
import com.fasterxml.aalto.async.{AsyncByteArrayScanner, AsyncStreamReaderImpl}
import com.fasterxml.aalto.stax.InputFactoryImpl
import ru.tinkoff.phobos.decoding.XmlDecoder.createStreamReader

/**
 * Typeclass for decoding XML document to an A value.
 *
 * XmlDecoder instance must exist only for types which are decoded as XML documents (only for root elements).
 *
 * XmlDecoder instance can be created
 *  - from ElementDecoder using functions in XmlDecoder object
 *  - by macros from ru.tinkoff.phobos.derivation.semiauto package
 *
 * This typeclass wraps ElementDecoder[A] and provides element name and Cursor.
 */
trait XmlDecoder[A]:
  val localname: String
  val elementdecoder: ElementDecoder[A]

  def decode(string: String, charset: String = "UTF-8"): Either[DecodingError, A] =
    decodeFromBytes(string.getBytes(charset), charset)

  def decodeFromBytes(bytes: Array[Byte], charset: String = "UTF-8"): Either[DecodingError, A] = 
    val sr: XmlStreamReader = createStreamReader(charset)

    sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
    sr.getInputFeeder.endOfInput()
    val cursor = new Cursor(sr)
    while {
      cursor.next()
      cursor.getEventType() == XMLStreamConstants.DTD || cursor.getEventType() == XMLStreamConstants.START_DOCUMENT
    } do ()

    elementdecoder
      .decodeAsElement(cursor, localname)
      .result(cursor.history)
  

  def decodeFromFoldable[F[_]: Foldable](f: F[Array[Byte]], charset: String = "UTF-8"): Either[DecodingError, A] = 
    val sr: XmlStreamReader = createStreamReader(charset)
    val cursor              = new Cursor(sr)

    val a = Foldable[F].foldLeft(f, elementdecoder): 
      (decoder: ElementDecoder[A], bytes: Array[Byte]) =>
        sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
        while {
          cursor.next()
          cursor.getEventType() == XMLStreamConstants.DTD || cursor.getEventType() == XMLStreamConstants.START_DOCUMENT
        } do ()
  
        if decoder.result(cursor.history).isRight then
          decoder
        else
          decoder.decodeAsElement(cursor, localname)
    sr.getInputFeeder.endOfInput()
    a.result(cursor.history)
  
end XmlDecoder

object XmlDecoder:

  def createStreamReader(charset: String): XmlStreamReader = 
    val inputFactory = new InputFactoryImpl
    val cfg          = inputFactory.getNonSharedConfig(null, null, null, false, false)
    cfg.setActualEncoding(charset)
    cfg.doReportCData(false)
    AsyncStreamReaderImpl[AsyncByteArrayFeeder](AsyncByteArrayScanner(cfg))
  

  def apply[A](using instance: XmlDecoder[A]): XmlDecoder[A] = instance

  def fromElementDecoder[A](localName: String)(
    using elementDecoder: ElementDecoder[A]): XmlDecoder[A] =
    new XmlDecoder[A]:
      val localname: String = localName
      val elementdecoder: ElementDecoder[A] = elementDecoder

end XmlDecoder