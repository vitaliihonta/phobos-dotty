package ru.tinkoff.phobos.derivation

import scala.quoted.QuoteContext

class QuotedOps(val quoteContext: QuoteContext):
  case class CaseClassParam(
    localName: String,
    xmlName: String,
    namespaceUri: String,
    paramType: quoteContext.tasty.Type,
    category: ParamCategory
  ):

    override def toString: String = s"CaseClassParam($localName, $xmlName, $namespaceUri, $paramType, $category)"

  end CaseClassParam
end QuotedOps