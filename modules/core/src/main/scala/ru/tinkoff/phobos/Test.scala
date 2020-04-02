package ru.tinkoff.phobos

import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.encoding._

case class Foo(@attr someName: Int, @attr @renamed("other") someOther: String, @text c: Double)
  derives ElementEncoder

case class Bar(@renamed("top_name") someTopName: String, someFoo: Foo, e: Char)
  derives XmlEncoder

case class Recursive(x: Option[Recursive], y: Int) derives ElementEncoder, XmlEncoder

@main def show = 
  val bar = Bar("d value", Foo(1, "b balue", 3.0), 'x')
  val string = bar.encode()
  println(string)

  val rec = Recursive(
    Some(
      Recursive(
        Some(Recursive(None, 1)),
        2
      )
    ),
    3
  )
  val string2 = rec.encode()
  println(string2)
end show
