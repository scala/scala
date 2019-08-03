/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal
package tpe

import scala.annotation.tailrec

private[internal] trait SymbolMaps {
  self: SymbolTable =>

  /**
    * A SymbolMap of T is just a finite mapping from Symbols to elements of a generic type T.
    * This is a restricted version of the maps from the Scala collections.
    * We want to allow functions that are just constants, functions, or access on an array. 
    */
  abstract class SymbolMap[+T] {
    def isEmpty: Boolean
    def hasTermKey: Boolean
    def find(sym: Symbol): Option[T]
    def hasKey(sym: Symbol): Boolean
  }

  class SingletonSM[+T](key: Symbol, value: T) extends SymbolMap[T]{
    override def isEmpty: Boolean = false
    override def hasTermKey: Boolean = key.isTerm
    override def find(sym: Symbol): Option[T] = if (key eq sym) Some(value) else None
    override def hasKey(sym: Symbol): Boolean = key eq sym
  }

  class MapSM[+T](env: scala.collection.Map[Symbol, T]) extends SymbolMap[T]{
    override final def isEmpty: Boolean = env.isEmpty
    override final val hasTermKey: Boolean = env.keysIterator.exists(_.isTerm)
    override def find(sym: Symbol): Option[T] = env.get(sym)
    override final def hasKey(sym: Symbol): Boolean = env.contains(sym)
  }

  abstract class KeysListSM[+T](from: List[Symbol]) extends SymbolMap[T] {
    private[this] var fromHasTermSymbol = false
    private[this] var fromMin = Int.MaxValue
    private[this] var fromMax = Int.MinValue
    private[this] var fromSize = 0
    from.foreach {
      sym =>
        fromMin = math.min(fromMin, sym.id)
        fromMax = math.max(fromMax, sym.id)
        fromSize += 1
        if (sym.isTerm) fromHasTermSymbol = true
    }

    final def hasTermKey: Boolean = fromHasTermSymbol

    /** Are `sym` and `sym1` the same? Can be tuned by subclasses. */
    protected def matches(sym: Symbol, sym1: Symbol): Boolean = sym eq sym1

    final def hasKey(sym: Symbol): Boolean = {
      // OPT Try cheap checks based on the range of symbol ids in from first.
      //     Equivalent to `from.contains(sym)`
      val symId = sym.id
      val fromMightContainSym = symId >= fromMin && symId <= fromMax
      fromMightContainSym && (
        symId == fromMin || symId == fromMax || (fromSize > 2 && from.contains(sym))
      )
    }

    override final def isEmpty: Boolean = from.isEmpty
  }

  class KeysConstantSM[+T](from: List[Symbol], const: T) extends KeysListSM[T](from) {
    def find(sym: Symbol): Option[T] =
      if (from.exists(matches(_, sym))) Some(const) else None
  }

  class KeysFunctionSM[+T](from: List[Symbol], fun: Symbol => T) extends KeysListSM[T](from) {
    def find(sym: Symbol): Option[T] =
      if (from.exists(matches(_, sym))) Some(fun(sym)) else None
  }

  class ZipSM[+T](val from: List[Symbol], val to: List[T]) extends KeysListSM[T](from) {
    // OPT this check was 2-3% of some profiles, demoted to -Xdev
    if (isDeveloper) assert(sameLength(from, to), "Unsound substitution from "+ from +" to "+ to)

    def find(sym: Symbol): Option[T] = {
      @tailrec def loop(from: List[Symbol], to: List[T]): Option[T] =
        if (from.isEmpty) None
        else if (matches(from.head, sym)) Some(to.head)
        else loop(from.tail, to.tail)
      loop(from, to)
    }
  }

  class ZippedMapSM[A, +T](from: List[Symbol], to: List[A], toFun: A => T) extends KeysListSM[T](from){
    def find(sym: Symbol): Option[T] = {
      @tailrec def loop(from: List[Symbol], to: List[A]): Option[T] =
        if (from.isEmpty) None
        else if (matches(from.head, sym)) Some(toFun(to.head))
        else loop(from.tail, to.tail)
      loop(from, to)
    }
  }

  class ZippedContraMappedSM[A, T](fromFun: A => Symbol, from: List[A], to: List[T]) extends SymbolMap[T] {
    private[this] var fromHasTermSymbol = false
    private[this] var fromMin = Int.MaxValue
    private[this] var fromMax = Int.MinValue
    private[this] var fromSize = 0
    from.foreach { a =>
      val sym = fromFun(a)
      fromMin = math.min(fromMin, sym.id)
      fromMax = math.max(fromMax, sym.id)
      fromSize += 1
      if (sym.isTerm) fromHasTermSymbol = true
    }

    final def hasTermKey: Boolean = fromHasTermSymbol

    /** Are `sym` and `sym1` the same? Can be tuned by subclasses. */
    protected def matches(sym: Symbol, sym1: Symbol): Boolean = sym eq sym1

    final def hasKey(sym: Symbol): Boolean = {
      // OPT Try cheap checks based on the range of symbol ids in from first.
      //     Equivalent to `from.contains(sym)`
      val symId = sym.id
      val fromMightContainSym = symId >= fromMin && symId <= fromMax
      fromMightContainSym && (
        symId == fromMin || symId == fromMax || (fromSize > 2 && from.contains(sym))
      )
    }

    override final def isEmpty: Boolean = from.isEmpty

    override def find(sym: Symbol): Option[T] = {
      @tailrec def loop(from: List[A], to: List[T]): Option[T] =
        if (from.isEmpty) None
        else if (matches(fromFun(from.head), sym)) Some(to.head)
        else loop(from.tail, to.tail)
      loop(from, to)
    }
  }

  class TypeVarsSM(typeVars: List[TypeVar]) extends SymbolMap[TypeVar] {
    private[this] var hasTermSymbol = false
    private[this] var fromMin = Int.MaxValue
    private[this] var fromMax = Int.MinValue
    private[this] var fromSize = 0

    typeVars.foreach { tvar =>
      val sym = tvar.origin.typeSymbol
      fromMin = math.min(fromMin, sym.id)
      fromMax = math.max(fromMax, sym.id)
      fromSize += 1
      if (sym.isTerm) hasTermSymbol = true
    }

    def isEmpty: Boolean = typeVars.isEmpty
    def hasTermKey: Boolean = hasTermSymbol
    def find(sym: Symbol): Option[TypeVar] =
      if (fromMin <= sym.id && sym.id <= fromMax) {
        var tvs = typeVars
        var res: Option[TypeVar] = None
        while (res.isEmpty && !tvs.isEmpty){
          val tv = tvs.head
          if (tv.origin.typeSymbol eq sym) res = Some(tv)
          tvs = tvs.tail
        }
        res
      } else None

    def hasKey(sym: Symbol): Boolean =
      fromMin <= sym.id && sym.id <= fromMax && (fromSize == 1 || ! find(sym).isEmpty)
  }

}
