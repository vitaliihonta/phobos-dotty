package ru.tinkoff.phobos

import ru.tinkoff.phobos.derivation.FetchGroup
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.encoding._

case class Foo(@attr someName: Int, @attr @renamed("other") someOther: String, @text c: Double)
  derives ElementEncoder

case class Bar(@renamed("top_name") someTopName: String, someFoo: Foo, e: Char)
  derives XmlEncoder

@main def show = 
  val bar = Bar("d value", Foo(1, "b balue", 3.0), 'x')
  val expected = """<?xml version='1.0' encoding='UTF-8'?><ru.tinkoff.phobos.Bar><top_name>d value</top_name><someFoo someName="1" other="b balue">3.0</someFoo><e>x</e></ru.tinkoff.phobos.Bar>"""
  val string = XmlEncoder[Bar].encode(bar)
  println(string)
  println(string == expected)
end show
