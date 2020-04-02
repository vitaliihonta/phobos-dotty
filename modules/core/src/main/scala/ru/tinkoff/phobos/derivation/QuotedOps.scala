package ru.tinkoff.phobos.derivation

import scala.quoted.QuoteContext

class QuotedOps(val quoteContext: QuoteContext):
  case class CaseClassParam(
    localName: String,
    xmlName: String,
    paramType: quoteContext.tasty.Type,
    category: ParamCategory
  ):

    override def toString: String = s"CaseClassParam($localName, $xmlName, $paramType, $category)"

  end CaseClassParam
end QuotedOps