package ru.tinkoff.phobos.encoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.{Contravariant, Foldable}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import ru.tinkoff.phobos.derivation._
import scala.quoted.{Expr, Type, QuoteContext}


/**
 * Warning! This is an internal API which may change in future.
 * Do not implement or use this trait directly unless you know what you are doing.
 *
 * Use XmlEncoder for encoding.
 *
 * ElementEncoder instance must exist for every type encoded to XML element.
 *
 * ElementEncoder instance can be created
 *  - from existing instance by using .contramap (mostly used for "simple" types);
 *  - by macros from ru.tinkoff.phobos.derivation.semiauto package (for case classes and sealed traits).
 *
 *
 * This typeclass describes process of encoding some A value to XML document. Name of the element is
 * not defined in typeclass, it should be passed in encodeAsElement method.
 */
trait ElementEncoder[A]:
  self =>
  def encodeAsElement(a: A, sw: PhobosStreamWriter, localName: String): Unit

  def contramap[B](f: B => A): ElementEncoder[B] =
    new ElementEncoder[B]:
      def encodeAsElement(b: B, sw: PhobosStreamWriter, localName: String): Unit =
        self.encodeAsElement(f(b), sw, localName)

end ElementEncoder

object ElementEncoder:
  given encoderContravariant as Contravariant[ElementEncoder] =
    new Contravariant[ElementEncoder]:
      def contramap[A, B](fa: ElementEncoder[A])(f: B => A): ElementEncoder[B] = fa.contramap(f)

  /**
    * Instances
    */
  given stringEncoder as ElementEncoder[String] =
    new ElementEncoder[String]:
      def encodeAsElement(a: String, sw: PhobosStreamWriter, localName: String): Unit =
        sw.writeStartElement(localName)
        sw.writeCharacters(a)
        sw.writeEndElement()
  end stringEncoder

  given unitEncoder as ElementEncoder[Unit] =
    new ElementEncoder[Unit]:
      def encodeAsElement(a: Unit, sw: PhobosStreamWriter, localName: String): Unit = ()

  given booleanEncoder as ElementEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  given javaBooleanEncoder as ElementEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  given charEncoder as ElementEncoder[Char]                           = stringEncoder.contramap(_.toString)
  given javaCharacterEncoder as ElementEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  given floatEncoder as ElementEncoder[Float]                         = stringEncoder.contramap(_.toString)
  given javaFloatEncoder as ElementEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  given doubleEncoder as ElementEncoder[Double]                       = stringEncoder.contramap(_.toString)
  given javaDoubleEncoder as ElementEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  given byteEncoder as ElementEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  given javaByteEncoder as ElementEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  given shortEncoder as ElementEncoder[Short]                         = stringEncoder.contramap(_.toString)
  given javaShortEncoder as ElementEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  given intEncoder as ElementEncoder[Int]                             = stringEncoder.contramap(_.toString)
  given javaIntegerEncoder as ElementEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  given longEncoder as ElementEncoder[Long]                           = stringEncoder.contramap(_.toString)
  given javaLongEncoder as ElementEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  given bigIntEncoder as ElementEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  given javaBigIntegerEncoder as ElementEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  given bigDecimalEncoder as ElementEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  given javaBigDecimalEncoder as ElementEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  given UUIDEncoder as ElementEncoder[UUID] = stringEncoder.contramap(_.toString)

  given base64Encoder as ElementEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  given optionEncoder[A](using encoder: ElementEncoder[A]) as ElementEncoder[Option[A]] =
    new ElementEncoder[Option[A]]:
      def encodeAsElement(a: Option[A], sw: PhobosStreamWriter, localName: String): Unit =
        a.foreach(encoder.encodeAsElement(_, sw, localName))

  given foldableEncoder[F[_]: Foldable, A](using encoder: ElementEncoder[A]) as ElementEncoder[F[A]] =
    new ElementEncoder[F[A]]:
      def encodeAsElement(vs: F[A], sw: PhobosStreamWriter, localName: String): Unit =
        Foldable[F].foldLeft(vs, ())((_, a) => encoder.encodeAsElement(a, sw, localName))

  given iteratorEncoder[A](using encoder: ElementEncoder[A]) as ElementEncoder[Iterator[A]] =
    new ElementEncoder[Iterator[A]]:
      def encodeAsElement(vs: Iterator[A],
                          sw: PhobosStreamWriter,
                          localName: String): Unit =
        vs.foreach(a => encoder.encodeAsElement(a, sw, localName))

  given seqEncoder[A: ElementEncoder] as ElementEncoder[Seq[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given setEncoder[A: ElementEncoder] as ElementEncoder[Set[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given listEncoder[A: ElementEncoder] as ElementEncoder[List[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given vectorEncoder[A: ElementEncoder] as ElementEncoder[Vector[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given chainEncoder[A: ElementEncoder] as ElementEncoder[Chain[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  given nonEmptyListEncoder[A: ElementEncoder] as ElementEncoder[NonEmptyList[A]] =
    listEncoder[A].contramap(_.toList)

  given nonEmptyVectorEncoder[A: ElementEncoder] as ElementEncoder[NonEmptyVector[A]] =
    vectorEncoder[A].contramap(_.toVector)

  given nonEmptySetEncoder[A: ElementEncoder] as ElementEncoder[NonEmptySet[A]] =
    setEncoder[A].contramap(_.toSortedSet)

  given nonEmptyChainEncoder[A: ElementEncoder] as ElementEncoder[NonEmptyChain[A]] =
    chainEncoder[A].contramap(_.toChain)

  given localDateTimeEncoder as ElementEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  given zonedDateTimeEncoder as ElementEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  given localDateEncoder as ElementEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  given localTimeEncoder as ElementEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
  
  def deriveImpl[T](using t: Type[T], q: QuoteContext): Expr[ElementEncoder[T]] =
    given ops as QuotedOps = QuotedOps(q)
    import q.tasty
    import q.tasty.{Type => _, _}
    
    val attrType = typeOf[AttributeEncoder[_]] match
      case x: AppliedType => x.tycon
    val textType = typeOf[TextEncoder[_]] match
      case x: AppliedType => x.tycon
    val elementType = typeOf[ElementEncoder[_]] match
      case x: AppliedType => x.tycon

    val stringType = typeOf[String]
    val params = FetchGroup.fetchProduct[T]
    val groups = params.groupBy(_.category)


    val encodeAsAttributeSym = Symbol.newMethod(Symbol.noSymbol, "encodeAsAttribute", typeOf[Unit])
    val encodeAsTextSym = Symbol.newMethod(Symbol.noSymbol, "encodeAsText", typeOf[Unit])
    val encodeAsElementSym = Symbol.newMethod(Symbol.noSymbol, "encodeAsElement", typeOf[Unit])

    def encodeAttributes(a: Term, sw: Term) = groups.getOrElse(ParamCategory.attribute, Nil).map: 
      param =>
        val p = param.paramType.asInstanceOf[tasty.TypeOrBounds]
        val attributeEncoder = AppliedType(attrType, List(p))
        val encoder = searchImplicit(attributeEncoder) match
          case t: Term => t
        Apply(
          encoder.select(encodeAsAttributeSym),
          List[Term](
            a.select(Symbol.newMethod(Symbol.noSymbol, param.localName, param.paramType.asInstanceOf[tasty.Type])), 
            sw, 
            Literal(Constant(param.xmlName))
          )
        )
    
    def encodeText(a: Term, sw: Term) = groups.getOrElse(ParamCategory.text, Nil).map: 
      param =>
        val p = param.paramType.asInstanceOf[tasty.TypeOrBounds]
        val textEncoder = AppliedType(textType, List(p))
        val encoder = searchImplicit(textEncoder) match
          case t: Term => t
        Apply(
          encoder.select(encodeAsTextSym),
          List[Term](
            a.select(Symbol.newMethod(Symbol.noSymbol, param.localName, param.paramType.asInstanceOf[tasty.Type])), 
            sw
          )
        )

    def encodeElements(a: Term, sw: Term) = groups.getOrElse(ParamCategory.element, Nil).map:
      param =>
        val p = param.paramType.asInstanceOf[tasty.TypeOrBounds]
        val elementEncoder = AppliedType(elementType, List(p))
        val encoder = searchImplicit(elementEncoder) match
          case t: Term => t
        Apply(
          encoder.select(encodeAsElementSym),
          List[Term](
            a.select(Symbol.newMethod(Symbol.noSymbol, param.localName, param.paramType.asInstanceOf[tasty.Type])), 
            sw, 
            Literal(Constant(param.xmlName))
          )
        )

        
    def expr(aExpr: Expr[T], swExpr: Expr[PhobosStreamWriter]) = 
      val a = aExpr.unseal
      val sw = swExpr.unseal
      Expr.ofList(
        encodeAttributes(a, sw).map(_.seal) ++ 
        encodeText(a, sw).map(_.seal) ++ 
        encodeElements(a, sw).map(_.seal)
      )

    val x = '{
        new ElementEncoder[$t]:
          override def encodeAsElement(
            a: $t,
            sw: PhobosStreamWriter,
            localName: String
          ): Unit = 
            sw.writeStartElement(localName)
            ${expr('a, 'sw)}
            sw.writeEndElement()
      }
      // println(x.show)
      x
  end deriveImpl

  inline given derived[T] as ElementEncoder[T] = ${deriveImpl[T]}

end ElementEncoder
