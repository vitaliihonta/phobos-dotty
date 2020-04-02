package ru.tinkoff.phobos.encoding

import java.io.ByteArrayOutputStream
import org.codehaus.stax2.XMLStreamWriter2
import cats.syntax.option._
import com.fasterxml.aalto.stax.OutputFactoryImpl

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
trait XmlEncoder[A]:
  val localname: String
  val elementencoder: ElementEncoder[A]

  def (a: A).encode(charset: String = "UTF-8"): String =
    String(a.encodeToBytes(charset), charset)

  def (a: A).encodeToBytes(charset: String = "UTF-8"): Array[Byte] = 
    val os      = new ByteArrayOutputStream
    val factory = new OutputFactoryImpl
    factory.setProperty("javax.xml.stream.isRepairingNamespaces", true)
    val sw = new PhobosStreamWriter(factory.createXMLStreamWriter(os, charset).asInstanceOf[XMLStreamWriter2])
    sw.writeStartDocument()
    elementencoder.encodeAsElement(a, sw, localname)
    sw.writeEndDocument()
    sw.flush()
    sw.close()
    os.toByteArray
  
end XmlEncoder

object XmlEncoder:

  def apply[A](using instance: XmlEncoder[A]): XmlEncoder[A] = instance

  import scala.quoted._
  import ru.tinkoff.phobos.derivation.raiseError
  
  inline given derived[A] as XmlEncoder[A] = 
    ${ derivedImpl[A] }

  def derivedImpl[A](using q: QuoteContext, A: Type[A]): Expr[XmlEncoder[A]] = 
    val a = Expr(A.show)
    val encoder = Expr.summon[ElementEncoder[A]] getOrElse raiseError(s"XmlEncoder requires ElementEncoder for ${A.show}")
    '{
      new XmlEncoder[$A]:
        override val localname: String                  = ${a}
        override val elementencoder: ElementEncoder[$A] = $encoder
     }
  end derivedImpl
end XmlEncoder
