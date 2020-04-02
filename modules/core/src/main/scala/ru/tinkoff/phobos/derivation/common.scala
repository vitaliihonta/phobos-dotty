package ru.tinkoff.phobos.derivation

import scala.quoted.QuoteContext

def raiseError(msg: =>String)(using q: QuoteContext): Nothing = 
    q.tasty.error(msg, q.tasty.rootPosition)
    ???
end raiseError

class Defered[A](val get: () => A)