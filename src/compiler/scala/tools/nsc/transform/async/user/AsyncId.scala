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

package scala.tools.nsc.transform.async
package user

import scala.language.experimental.macros
import scala.reflect.api
import scala.reflect.internal.SymbolTable
import scala.reflect.macros.whitebox.Context

object AsyncId extends AsyncBase {
  lazy val futureSystem = IdentityFutureSystem
  type FS = IdentityFutureSystem.type

  def async[T](body: => T) = macro asyncIdImpl[T]

  def asyncIdImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = asyncImpl[T](c)(body)(c.literalUnit)
}

object AsyncTestLV extends AsyncBase {
  lazy val futureSystem = IdentityFutureSystem
  type FS = IdentityFutureSystem.type

  def async[T](body: T) = macro asyncIdImpl[T]

  def asyncIdImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = asyncImpl[T](c)(body)(c.literalUnit)

  var log: List[(String, Any)] = Nil
  def assertNulledOut(a: Any): Unit = assert(log.exists(_._2 == a), AsyncTestLV.log)
  def assertNotNulledOut(a: Any): Unit = assert(!log.exists(_._2 == a), AsyncTestLV.log)
  def clear(): Unit = log = Nil

  def apply(name: String, v: Any): Unit =
    log ::= (name -> v)

  protected[async] override def nullOut(u: api.Universe)(name: u.Expr[String], v: u.Expr[Any]): u.Expr[Unit] =
    u.reify { AsyncTestLV(name.splice, v.splice) }
}

/**
 * A trivial implementation of [[FutureSystem]] that performs computations
 * on the current thread. Useful for testing.
 */
class Box[A] {
  var a: A = _
}
object IdentityFutureSystem extends FutureSystem {
  type Prom[A] = Box[A]

  type Fut[A] = A
  type ExecContext = Unit
  type Tryy[A] = scala.util.Try[A]

  def mkOps(u: SymbolTable, isPastErasure: Boolean = false): Ops[u.type] = new IdentityOps[u.type](u, isPastErasure)
  class IdentityOps[Universe <: SymbolTable](u0: Universe, isPastErasure: Boolean) extends Ops[Universe](u0, isPastErasure) {
    import u._

    def promType(tp: Type): Type = phasedAppliedType(weakTypeOf[Box[_]], tp)
    def tryType(tp: Type): Type = phasedAppliedType(weakTypeOf[util.Try[_]], tp)

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = {
      val newProm = reify { new Prom[A]() }
      if (isPastErasure)
        Expr[Prom[A]](newProm.tree match {
          // drop type apply
          case ap@Apply(sel@Select(nw@New(AppliedTypeTree(newProm, _)), ctor), args) =>
            treeCopy.Apply(ap, treeCopy.Select(sel, treeCopy.New(nw, newProm), ctor), args)
        })
      else newProm
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]) = {
      val expr = reify { prom.splice.a }
      if (isPastErasure) Expr[Fut[A]](Apply(expr.tree, Nil))
      else expr
    }

    def future[A: WeakTypeTag](t: Expr[A])(execContext: Expr[ExecContext]) = t

    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[Tryy[A] => B],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      fun.splice.apply(util.Success(future.splice))
      literalUnitExpr.splice
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[Tryy[A]]): Expr[Unit] = {
      val valueGet = reify { value.splice.get }
      reify {
        prom.splice.a = { if (isPastErasure) Expr[A](Apply(valueGet.tree, Nil)) else valueGet }.splice
        literalUnitExpr.splice
      }
    }

    def tryyIsFailure[A](tryy: Expr[Tryy[A]]): Expr[Boolean] = reify {
      tryy.splice.isFailure
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = {
      val expr = reify { tryy.splice.get }
      if (isPastErasure) Expr[A](Apply(expr.tree, Nil))
      else expr
    }

    def tryySuccess[A: WeakTypeTag](a: Expr[A]): Expr[Tryy[A]] = {
      val expr = reify { scala.util.Success[A](a.splice) }
      if (isPastErasure)
        Expr[Tryy[A]](expr.tree match {
          // drop type apply
          case ap@Apply(TypeApply(succ, _), args) => treeCopy.Apply(ap, succ, args)
        })
      else expr
    }
    def tryyFailure[A: WeakTypeTag](a: Expr[Throwable]): Expr[Tryy[A]] = {
      val expr = reify { scala.util.Failure[A](a.splice) }
      if (isPastErasure)
        Expr[Tryy[A]](expr.tree match {
          // drop type apply
          case ap@Apply(TypeApply(fail, _), args) => treeCopy.Apply(ap, fail, args)
        })
      else expr
    }
  }
}
