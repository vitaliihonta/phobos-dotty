package ru.tinkoff.phobos.derivation

/*
 * Copy-pasted from https://github.com/propensive/magnolia
 */

import scala.collection.mutable
import scala.quoted._

private[derivation] object CompileTimeState with

  sealed abstract class TypePath(path: String) with
    override def toString = path
  
  final case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  final case class ProductType(paramName: String, typeName: String)
    extends TypePath(s"parameter '$paramName' of product type $typeName")

  final case class ChainedImplicit(typeClassName: String, typeName: String)
    extends TypePath(s"chained implicit $typeClassName for type $typeName")

  final class Stack with
    private var frames = List.empty[Frame[_]]
    private val cache = mutable.Map.empty[Type[_], Expr[_]]
 
    def isEmpty = frames.isEmpty
    def nonEmpty = frames.nonEmpty
    def top() = frames.headOption
    def pop(): Unit = frames = frames drop 1
    def push(frame: Frame[_]): Unit = frames ::= frame

    def clear(): Unit =
      frames = Nil
      cache.clear()

    // def find(searchType: Type): Option[TermName] = frames.collectFirst {
    //   case Frame(_, tpe, term) if tpe =:= searchType => term
    // }

    def recurse[T](frame: Frame[T], searchType: Type[T])(fn: => Expr[T]): Expr[T] =
      push(frame)
      val result = cache.getOrElseUpdate(searchType, fn)
      pop()
      result.asInstanceOf[Expr[T]]

    def trace: List[TypePath] =
      frames.drop(1).zip(frames).collect:
        case (Frame(path, tp1, _), Frame(_, tp2, _))
          if !(tp1 == tp2) => path

    override def toString: String =
      frames.mkString(" a stack:\n", "\n", "\n")

    final case class Frame[T](path: TypePath, searchType: Type[T], term: String)

  object Stack with
    // Cheating to satisfy Singleton bound (which improves type inference).
    // private val dummyContext: blackbox.Context = null
    private val global = new Stack
    // private val workSet = mutable.Set.empty[blackbox.Context#Symbol]

    // def withContext(c: blackbox.Context)(fn: Stack=> c.Tree): c.Tree = {
    //   workSet += c.macroApplication.symbol
    //   val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
    //   try fn(global.asInstanceOf[Stack[c.type]])
    //   finally if (depth <= 1) {
    //     global.clear()
    //     workSet.clear()
    //   }
    // }
  end Stack
end CompileTimeState