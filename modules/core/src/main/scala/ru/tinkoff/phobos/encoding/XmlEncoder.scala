package ru.tinkoff.phobos.encoding

import java.io.ByteArrayOutputStream

import org.codehaus.stax2.XMLStreamWriter2
import cats.syntax.option._
import com.fasterxml.aalto.stax.OutputFactoryImpl
import ru.tinkoff.phobos.Namespace

/**
 * Typeclass for encoding XML document to an A value.
 *
 * XmlEncoder instance must exist only for types which are encoded as XML documents (only for root elements).
 *
 * XmlEncoder instance can be created
 *  - from ElementEncoder using functions in XmlEncoder object
 *  - by macros from ru.tinkoff.phobos.derivation.semiauto package
 *
 * This typeclass wraps ElementEncoder[A] and provides element name and StreamWriter.
 */
trait XmlEncoder[A] with
  val localname: String
  val namespaceuri: Option[String]
  val elementencoder: ElementEncoder[A]

  def encode(a: A, charset: String = "UTF-8"): String =
    String(encodeToBytes(a, charset), charset)

  def encodeToBytes(a: A, charset: String = "UTF-8"): Array[Byte] = 
    val os      = new ByteArrayOutputStream
    val factory = new OutputFactoryImpl
    factory.setProperty("javax.xml.stream.isRepairingNamespaces", true)
    val sw = factory.createXMLStreamWriter(os, charset).asInstanceOf[XMLStreamWriter2]
    sw.writeStartDocument()
    elementencoder.encodeAsElement(a, sw, localname, namespaceuri)
    sw.writeEndDocument()
    sw.flush()
    sw.close()
    os.toByteArray
  
end XmlEncoder

object XmlEncoder with

  def apply[A](using instance: XmlEncoder[A]): XmlEncoder[A] = instance

  def fromElementEncoder[A](localName: String, namespaceUri: Option[String])(
      using elementEncoder: ElementEncoder[A]): XmlEncoder[A] =
    new XmlEncoder[A] with
      val localname: String                 = localName
      val namespaceuri: Option[String]      = namespaceUri
      val elementencoder: ElementEncoder[A] = elementEncoder
  end fromElementEncoder

  def fromElementEncoder[A](localName: String)(implicit elementEncoder: ElementEncoder[A]): XmlEncoder[A] =
    fromElementEncoder(localName, None)

  def fromElementEncoderNs[A, NS](localName: String, namespaceInstance: NS)(implicit elementEncoder: ElementEncoder[A],
                                                                            namespace: Namespace[NS]): XmlEncoder[A] =
    fromElementEncoder(localName, namespace.getNamespace.some)

  def fromElementEncoderNs[A, NS](localName: String)(implicit elementEncoder: ElementEncoder[A],
                                                     namespace: Namespace[NS]): XmlEncoder[A] =
    fromElementEncoder(localName, namespace.getNamespace.some)
end XmlEncoder
