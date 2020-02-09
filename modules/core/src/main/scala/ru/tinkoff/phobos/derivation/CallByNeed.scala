package ru.tinkoff.phobos.derivation

/*
 * Copy-pasted from https://github.com/propensive/magnolia
 */

object CallByNeed with 
  def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a)

final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable with
  lazy val value: A =
    val result = eval()
    eval = null
    result
