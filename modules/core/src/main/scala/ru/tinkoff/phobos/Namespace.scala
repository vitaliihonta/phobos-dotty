package ru.tinkoff.phobos

/**
 * Typeclass for defining XML namespaces.
 *
 * Example of use:
 *
 *  case object nsi {
 *    implicit val ns: Namespace[nsi.type] = Namespace.mkInstance("example.com")
 *  }
 *
 * See package annotations for more convenient syntax.
 */
trait Namespace[T] with
  val getNamespace: String

object Namespace with
  def mkInstance[T](uri: String): Namespace[T] = new Namespace[T] with
    val getNamespace = uri

  def apply[T](implicit instance: Namespace[T]): Namespace[T] = instance
