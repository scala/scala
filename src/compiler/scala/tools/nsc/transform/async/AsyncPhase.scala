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

import scala.tools.nsc.Mode
import scala.tools.nsc.transform.{Transform, TypingTransformers}

abstract class AsyncPhase extends Transform with TypingTransformers  {
  private val asyncNames_ = new AsyncNames[global.type](global)
  import global._

  val phaseName: String = "async"
  override def enabled = true // TODO: should be off by default, enabled by flag
//  {
//    (currentRun.runDefinitions match {
//      case null => new definitions.RunDefinitions
//      case rd => rd
//    }).Async_async.exists
//  }



  object macroExpansion extends AsyncEarlyExpansion {
    val u: global.type = global
    val asyncBase = user.ScalaConcurrentAsync
    import treeInfo.Applied

    def fastTrackEntry: (Symbol, PartialFunction[Applied, scala.reflect.macros.contexts.Context { val universe: u.type } => Tree]) =
      (currentRun.runDefinitions.Async_async, {
        // def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro ???
        case app@Applied(_, resultTp :: Nil, List(asyncBody :: Nil, execContext :: Nil)) =>
          c => c.global.async.macroExpansion(c.global.analyzer.suppressMacroExpansion(app.tree), execContext, resultTp.tpe, c.internal.enclosingOwner)
      })

  }

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  // TODO: support more than the original late expansion tests
  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    abstract class AsyncTransformBase(asyncBase: user.AsyncBase) extends AsyncTransform(asyncBase) {
      val u: global.type = global
      import u._

      val asyncNames: AsyncNames[u.type] = asyncNames_

      def typecheck(tree: Tree): Tree = localTyper.typed(tree)
      def abort(pos: Position, msg: String): Nothing = {localTyper.context.reporter.error(pos, msg); ???}
      def error(pos: Position, msg: String): Unit = localTyper.context.reporter.error(pos, msg)

      val typingTransformers =
        new TypingTransformers {
          def callsiteTyper = localTyper
        }
    }

    object asyncTransformerConcurrent extends AsyncTransformBase(user.ScalaConcurrentAsync) {
      val Async_async = currentRun.runDefinitions.Async_async
      val Async_await = currentRun.runDefinitions.Async_await
    }

    lazy val uncurrier = new uncurry.UnCurryTransformer(unit)

    override def transform(tree: Tree): Tree =
      tree match {
        // {
        //    class stateMachine$async extends scala.runtime.AbstractFunction1 with Function0$mcV$sp {
        //      def apply(tr$async: scala.util.Try): Unit = { // symbol of this def is `applySym`, symbol of its param named "tr$async" is `trParamSym`
        //      ...
        //    }
        //    val stateMachine = ...
        //    ...
        // }
        case Block((cd@ClassDef(mods, tpnme.stateMachine, _, impl@Template(parents, self, stats))) :: (vd@ValDef(_, nme.stateMachine, tpt, _)) :: rest, expr) if tpt.tpe.typeSymbol == cd.symbol =>
          import asyncTransformerConcurrent._

          stats.collectFirst {
            case dd@DefDef(mods, name@nme.apply, tparams, vparamss@List(tr :: Nil), tpt, Block( Apply(asyncMacro, List(asyncBody, execContext)) :: Nil, Literal(Constant(())))) =>
              asyncTransform(asyncBody, dd.symbol, tr.symbol, execContext) match {
                case Some((newRhs, liftables)) =>
                  Right(treeCopy.DefDef(dd, mods, name, tparams, vparamss, tpt, newRhs).setType(null) /* need to retype */ :: liftables)
                case None =>
                  val thunkFun = localTyper.typedPos(asyncBody.pos)(Function(Nil, asyncBody)).asInstanceOf[Function]
                  thunkFun.body.changeOwner(dd.symbol, thunkFun.symbol)
                  thunkFun.setType(definitions.functionType(Nil, exitingTyper { futureSystemOps.tryTypeToResult(tr.symbol.info) })) // ugh (uncurry normally runs before erasure and wants a full function type)
                  Left(futureSystemOps.future(uncurrier.transformFunction(thunkFun), execContext))
              }
          } match {
            case Some(Left(simple)) => localTyper.typed(simple)
            case Some(Right(newStats@(newApply :: liftables))) =>
              val newTempl = treeCopy.Template(impl, parents, self, stats.filterNot(_.symbol == newApply.symbol) ::: newStats)
              treeCopy.Block(tree, localTyper.typedClassDef(treeCopy.ClassDef(cd, mods, tpnme.stateMachine, Nil, newTempl)) :: vd :: rest, expr)
          }
//        case ap@Apply(fun, rhs :: execContext :: Nil) if fun.symbol == asyncTransformerConcurrent.Async_async => transformAsyncStd(rhs, execContext)
        case tree                                                                     =>
          super.transform(tree)
      }

  }
}
