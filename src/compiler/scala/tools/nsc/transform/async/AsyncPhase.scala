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

import scala.collection.mutable
import scala.tools.nsc.transform.async.user.FutureSystem
import scala.tools.nsc.transform.{Transform, TypingTransformers}

abstract class AsyncPhase extends Transform with TypingTransformers with AsyncTransform {
  self =>
  import global._

  val asyncNames = new AsyncNames[global.type](global)
  protected val tracing = new Tracing

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
      case app@Applied(_, _, List(asyncBody :: Nil, execContext :: Nil)) =>
        c => c.global.async.macroExpansion.apply(c.callsiteTyper, asyncBody, execContext, asyncBody.tpe)
    })

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  final class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    private lazy val liftables = new mutable.AnyRefMap[Symbol, List[Tree]]()

    // Together, these transforms below target this tree shaps
    // {
    //    class $STATE_MACHINE extends ... {
    //      def $APPLY_METHOD(....) = {
    //      ...
    //      }.updateAttachment(FutureSystemAttachment(...))
    //    }
    // }
    //
    // The RHS of the method is transformed into a state machine with that transformation tailored by the
    // attached `FutureSystem`. Local val/var/def/class/object trees that are referred to from multiple states
    // are lifted into members of the enclosing class.
    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        case cd: ClassDef if liftables.contains(cd.symbol) =>
          deriveClassDef(cd)(impl => deriveTemplate(impl)(liftables.remove(cd.symbol).getOrElse(Nil) ::: _))

        case dd: DefDef if tree.hasAttachment[FutureSystemAttachment] =>
          val futureSystem = tree.getAndRemoveAttachment[FutureSystemAttachment].get.system
          val asyncBody = (dd.rhs: @unchecked) match {
            case blk@Block(stats, Literal(Constant(()))) => treeCopy.Block(blk, stats.init, stats.last).setType(stats.last.tpe)
          }

          val saved = currentTransformState
          atOwner(dd, dd.symbol) {
            val trSym = dd.vparamss.head.head.symbol
            val saved = currentTransformState
            currentTransformState = new AsyncTransformState[global.type](global, futureSystem, this, trSym, asyncBody.tpe)
            try {
              val (newRhs, liftableFields) = asyncTransform(asyncBody)
              liftables(dd.symbol.owner) = liftableFields
              deriveDefDef(dd)(_ => newRhs)
            } finally {
              currentTransformState = saved
            }
          }
        case tree => tree
      }
  }
}
