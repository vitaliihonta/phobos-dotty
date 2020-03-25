package ru.tinkoff.phobos.decoding

import java.io.Writer
import java.math.{BigDecimal, BigInteger}

import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.{Location, XMLStreamConstants}
import org.codehaus.stax2.typed.{Base64Variant, TypedArrayDecoder, TypedValueDecoder}
import org.codehaus.stax2.{AttributeInfo, LocationInfo}

/**
 * Warning! This is internal API which may change in future.
 * Do not this class directly unless you know what you are doing.
 *
 * Cursor is a wrapper around XmlStreamReader providing
 * information about position in XML document.
 */
class Cursor(private val sr: XmlStreamReader):
  export sr.{next => _, setFeature => _,  getFeature => _, _}

  private var historyStack: List[String] = Nil

  def next(): Int = 
    val next = sr.next()
    next match
      case XMLStreamConstants.START_ELEMENT =>
        historyStack = sr.getLocalName :: historyStack
      case XMLStreamConstants.END_ELEMENT =>
        historyStack = historyStack.tail
      case _ =>
    
    next
    
  def history: List[String] = historyStack
  def error(text: String): DecodingError = DecodingError(text, historyStack)
end Cursor
