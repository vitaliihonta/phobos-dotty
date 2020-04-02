package ru.tinkoff.phobos

import ru.tinkoff.phobos.derivation.FetchGroup
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.encoding.XmlEncoder

case class Foo(@attr bar: Int, @text s: String) derives XmlEncoder

@main def show = 
  val x = Foo(1, "lala")
  println(
    FetchGroup.get[Foo]
  )
end show