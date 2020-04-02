package ru.tinkoff.phobos.encoding

import java.math.BigInteger

import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.XMLStreamException
import org.codehaus.stax2.typed.Base64Variant
import org.codehaus.stax2.{XMLStreamLocation2, XMLStreamReader2, XMLStreamWriter2}
import org.codehaus.stax2.validation.{ValidationProblemHandler, XMLValidationSchema, XMLValidator}

final class PhobosStreamWriter(val sw: XMLStreamWriter2) extends XMLStreamWriter2:

  private var discriminatorLocalName: Option[String] = None
  private var discriminatorNamespace: Option[String] = None
  private var discriminatorValue: Option[String] = None

  def memorizeDiscriminator(namespaceUri: Option[String], localName: String, value: String): Unit =
    discriminatorNamespace = namespaceUri
    discriminatorLocalName = Some(localName)
    discriminatorValue = Some(value)

  export sw.{writeStartElement => _, _}

  override def writeStartElement(localName: String): Unit =
    sw.writeStartElement(localName: String)
    maybeWriteDiscriminator()

  override def writeStartElement(namespaceURI: String, localName: String): Unit =
    sw.writeStartElement(namespaceURI, localName)
    maybeWriteDiscriminator()

  override def writeStartElement(prefix: String, localName: String, namespaceURI: String): Unit = 
    sw.writeStartElement(prefix, localName, namespaceURI)
    maybeWriteDiscriminator()

  private def maybeWriteDiscriminator(): Unit = 
    (discriminatorNamespace, discriminatorLocalName, discriminatorValue) match
      case (None, None, None) =>
      case (None, Some(dLocalName), Some(dValue)) => sw.writeAttribute(dLocalName, dValue)
      case (Some(dNamespace), Some(dLocalName), Some(dValue)) => sw.writeAttribute(dNamespace, dLocalName, dValue)
      case state => throw new XMLStreamException(s"Unexpected discriminator names state: $state")

    discriminatorNamespace = None
    discriminatorLocalName = None
    discriminatorValue = None
end PhobosStreamWriter
