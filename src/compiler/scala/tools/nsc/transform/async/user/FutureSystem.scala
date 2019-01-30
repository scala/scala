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

package scala.tools.nsc.transform.async.user

import scala.language.higherKinds
import scala.reflect.internal.SymbolTable

/**
 * An abstraction over a future system.
 *
 * Used by the macro implementations in [[scala.async.AsyncBase]] to
 * customize the code generation.
 *
 * The API mirrors that of `scala.concurrent.Future`, see the instance
 * [[ScalaConcurrentFutureSystem]] for an example of how
 * to implement this.
 */
trait FutureSystem {
  /** A container to receive the final value of the computation */
  type Prom[A]
  /** A (potentially in-progress) computation */
  type Fut[A]
  /** An execution context, required to create or register an on completion callback on a Future. */
  type ExecContext
  /** Any data type isomorphic to scala.util.Try. */
  type Tryy[T]

  // We could do with just Universe <: reflect.api.Universe if it wasn't Expr and WeakTypeTag
  // (the api effectively doesn't let you call these factories since there's no way to get at the Tree- and TypeCreators)
  abstract class Ops[Universe <: SymbolTable](val u: Universe, val isPastErasure: Boolean) {
    import u._

    def Expr[T: WeakTypeTag](tree: Tree): Expr[T] = u.Expr[T](rootMirror, FixedMirrorTreeCreator(rootMirror, tree))
    def WeakTypeTag[T](tpe: Type): WeakTypeTag[T] = u.WeakTypeTag[T](rootMirror, FixedMirrorTypeCreator(rootMirror, tpe))

    def literalUnitExpr = Expr[Unit](if (isPastErasure) gen.mkAttributedRef(definitions.BoxedUnit_UNIT) else Literal(Constant(())))

    def phasedAppliedType(tycon: Type, tp: Type) = if (isPastErasure) tycon else appliedType(tycon, tp)

    def promType(tp: Type): Type
    def tryType(tp: Type): Type
    def stateMachineClassParents: List[Type] = Nil

    /** Create an empty promise */
    def createProm[A: WeakTypeTag]: Expr[Prom[A]]

    /** Extract a future from the given promise. */
    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]): Expr[Fut[A]]

    /** Construct a future to asynchronously compute the given expression */
    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]): Expr[Fut[A]]

    /** Register an call back to run on completion of the given future */
    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[Tryy[A] => B],
                         execContext: Expr[ExecContext]): Expr[Unit]

    def continueCompletedFutureOnSameThread = false

    /** Return `null` if this future is not yet completed, or `Tryy[A]` with the completed result
      * otherwise
      */
    def getCompleted[A: WeakTypeTag](future: Expr[Fut[A]]): Expr[Tryy[A]] =
      throw new UnsupportedOperationException("getCompleted not supported by this FutureSystem")

    /** Complete a promise with a value */
    def completeProm[A](prom: Expr[Prom[A]], value: Expr[Tryy[A]]): Expr[Unit]
    def completeWithSuccess[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[A]): Expr[Unit] = completeProm(prom, tryySuccess(value))

    def tryyIsFailure[A](tryy: Expr[Tryy[A]]): Expr[Boolean]

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A]
    def tryySuccess[A: WeakTypeTag](a: Expr[A]): Expr[Tryy[A]]
    def tryyFailure[A: WeakTypeTag](a: Expr[Throwable]): Expr[Tryy[A]]

    /** A hook for custom macros to transform the tree post-ANF transform */
    def postAnfTransform(tree: Block): Block = tree

    /** A hook for custom macros to selectively generate and process a Graphviz visualization of the transformed state machine */
    def dot(enclosingOwner: Symbol, macroApplication: Tree): Option[(String => Unit)] = None
  }

  def mkOps(u: SymbolTable, isPastErasure: Boolean = false): Ops[u.type]

  @deprecated("No longer honoured by the macro, all generated names now contain $async to avoid accidental clashes with lambda lifted names", "0.9.7")
  def freshenAllNames: Boolean = false
  def emitTryCatch: Boolean = true
  @deprecated("No longer honoured by the macro, all generated names now contain $async to avoid accidental clashes with lambda lifted names", "0.9.7")
  def resultFieldName: String = "result"
}

// TODO AM: test the erased version by running the remainder of the test suite post-posterasure (i.e., not LateExpansion, which tests AsyncId)
object ScalaConcurrentFutureSystem extends FutureSystem {
  import scala.concurrent._

  type Prom[A] = Promise[A]
  type Fut[A] = Future[A]
  type ExecContext = ExecutionContext
  type Tryy[A] = scala.util.Try[A]

  def mkOps(u: SymbolTable, isPastErasure: Boolean = false): Ops[u.type] = new ScalaConcurrentOps[u.type](u, isPastErasure)
  class ScalaConcurrentOps[Universe <: SymbolTable](u0: Universe, isPastErasure: Boolean) extends Ops[Universe](u0, isPastErasure) {
    import u._

    def promType(tp: Type): Type = phasedAppliedType(weakTypeOf[Promise[_]], tp)
    def tryType(tp: Type): Type = phasedAppliedType(weakTypeOf[scala.util.Try[_]], tp)

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = {
      val newProm = reify { Promise[A]() }
      if (isPastErasure)
        Expr[Prom[A]](newProm.tree match { // drop type apply
          case ap@Apply(TypeApply(prom, _), args) => treeCopy.Apply(ap, prom, args)
        })
      else newProm
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]) = reify {
      prom.splice.future
    }

    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]) = {
      val expr = reify { Future(a.splice)(execContext.splice) }
      if (isPastErasure)
        expr.tree match {
          case ap@Apply(Apply(fut, a), execCtx) => Expr[Future[A]](treeCopy.Apply(ap, fut, a ++ execCtx))
        }
      else expr
    }

    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => B],
                         execContext: Expr[ExecContext]): Expr[Unit] = {
      val expr = reify { future.splice.onComplete(fun.splice)(execContext.splice) }
      if (isPastErasure)
        expr.tree match {
          case ap@Apply(Apply(fut, fun), execCtx) => Expr[Unit](treeCopy.Apply(ap, fut, fun ++ execCtx))
        }
      else expr
    }

    override def continueCompletedFutureOnSameThread: Boolean = true

    override def getCompleted[A: WeakTypeTag](future: Expr[Fut[A]]): Expr[Tryy[A]] = {
      val valueGet = reify { future.splice.value.get }
      val isCompleted = reify { future.splice.isCompleted }
      reify {
        if ({ if (isPastErasure) Expr[Boolean](Apply(isCompleted.tree, Nil)) else isCompleted }.splice)
          { if (isPastErasure) Expr[Tryy[A]](Apply(valueGet.tree, Nil)) else valueGet }.splice else null
      }
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
      prom.splice.complete(value.splice)
      literalUnitExpr.splice
    }

    def tryyIsFailure[A](tryy: Expr[scala.util.Try[A]]): Expr[Boolean] = {
      val expr = reify { tryy.splice.isFailure }
      if (isPastErasure) Expr[Boolean](Apply(expr.tree, Nil))
      else expr
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = {
      val expr = reify { tryy.splice.get }
      if (isPastErasure) Expr[A](Apply(expr.tree, Nil))
      else expr
    }

    def tryySuccess[A: WeakTypeTag](a: Expr[A]): Expr[Tryy[A]] = {
      val expr = reify { scala.util.Success[A](a.splice) }
      if (isPastErasure)
        Expr[Tryy[A]](expr.tree match { // drop type apply
          case ap@Apply(TypeApply(succ, _), args) => treeCopy.Apply(ap, succ, args)
        })
      else expr
    }
    def tryyFailure[A: WeakTypeTag](a: Expr[Throwable]): Expr[Tryy[A]] = {
      val expr = reify { scala.util.Failure[A](a.splice) }
      if (isPastErasure)
        Expr[Tryy[A]](expr.tree match { // drop type apply
          case ap@Apply(TypeApply(fail, _), args) => treeCopy.Apply(ap, fail, args)
        })
      else expr
    }
  }
}
