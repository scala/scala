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

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Mode
import scala.tools.nsc.transform.async.user.{FutureSystem, ScalaConcurrentFutureSystem}
import scala.tools.nsc.transform.{Transform, TypingTransformers}

abstract class AsyncPhase extends Transform with TypingTransformers with AsyncTransform{
  self =>
  import global._

  val asyncNames = new AsyncNames[global.type](global)

  val phaseName: String = "async"
  override def enabled = true // TODO: should be off by default, enabled by flag
//  {
//    (currentRun.runDefinitions match {
//      case null => new definitions.RunDefinitions
//      case rd => rd
//    }).Async_async.exists
//  }
  final class FutureSystemAttachment(val system: FutureSystem) extends PlainAttachment

  object macroExpansion extends AsyncEarlyExpansion {
    val global: self.global.type = self.global
  }

  import treeInfo.Applied
  def fastTrackEntry: (Symbol, PartialFunction[Applied, scala.reflect.macros.contexts.Context { val universe: self.global.type } => Tree]) =
    (currentRun.runDefinitions.Async_async, {
      // def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro ???
      case app@Applied(_, resultTp :: Nil, List(asyncBody :: Nil, execContext :: Nil)) =>
        c => c.global.async.macroExpansion.apply(c.callsiteTyper, asyncBody, execContext, resultTp.tpe, c.internal.enclosingOwner)
    })

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  // TODO: support more than the original late expansion tests
  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    lazy val uncurrier = new uncurry.UnCurryTransformer(unit)
    lazy val eraser = new erasure.ErasureTransformer(unit)

    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        // {
        //    class stateMachine$async extends scala.runtime.AbstractFunction1 with Function0$mcV$sp {
        //      def apply(tr$async: scala.util.Try): Unit = { // symbol of this def is `applySym`, symbol of its param named "tr$async" is `trParamSym`
        //      ...
        //    }
        //    val stateMachine = ...
        //    ...
        // }
        case tree if tree.hasAttachment[FutureSystemAttachment] =>
          val saved = currentTransformState
          val futureSystem = tree.getAndRemoveAttachment[FutureSystemAttachment].get.system
          val newState = new AsyncTransformState[global.type](global, futureSystem, unit, this)
          currentTransformState = newState
          try tree match {
            case blk@Block((temp@ValDef(_, nme.execContextTemp, _, execContext)) :: (cd@ClassDef(mods, tpnme.stateMachine, _, impl@Template(parents, self, stats))) :: (vd@ValDef(_, nme.stateMachine, tpt, _)) :: rest, expr) if tpt.tpe.typeSymbol == cd.symbol =>
              val ((dd: DefDef) :: Nil, others) = stats.partition {
                case dd@DefDef(mods, nme.apply, _, List(tr :: Nil), _, _) => !dd.symbol.isBridge
                case _ => false
              }
              val asyncBody = (dd.rhs: @unchecked) match {
                case Block(stats, Literal(Constant(()))) => Block(stats.init, stats.last)
              }
              val (newRhs, liftables) = asyncTransform(asyncBody, dd.symbol, dd.vparamss.head.head.symbol)

              val newApply = deriveDefDef(dd)(_ => newRhs).setType(null) /* need to retype */
              val newStats = new ListBuffer[Tree]
              newStats ++= others
              newStats += newApply
              newStats ++= liftables
              val newTempl = treeCopy.Template(impl, parents, self, newStats.toList)
              treeCopy.Block(tree, temp :: localTyper.typedClassDef(treeCopy.ClassDef(cd, mods, tpnme.stateMachine, Nil, newTempl)) :: vd :: rest, expr)
          } finally {
            currentTransformState = saved
          }
        case tree => tree
      }
  }
}
