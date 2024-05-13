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
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.reflect.internal.util.{SourceFile, NoSourceFile}

abstract class AsyncPhase extends Transform with TypingTransformers with AnfTransform with Lifter with LiveVariables {
  self =>
  import global._

  private[async] var currentTransformState: AsyncTransformState = _
  private[async] val asyncNames = new AsyncNames[global.type](global)
  protected[async] val tracing = new Tracing

  val phaseName: String = "async"
  override def enabled: Boolean = settings.async

  private final case class AsyncAttachment(awaitSymbol: Symbol, postAnfTransform: Block => Block,
                                           stateDiagram: ((Symbol, Tree) => Option[String => Unit]),
                                           allowExceptionsToPropagate: Boolean) extends PlainAttachment

  def hasAsyncAttachment(dd: DefDef) = dd.hasAttachment[AsyncAttachment]

  // Optimization: avoid the transform altogether if there are no async blocks in a unit.
  private val sourceFilesToTransform = perRunCaches.newSet[SourceFile]()
  private val awaits: mutable.Set[Symbol] = perRunCaches.newSet[Symbol]()

  /**
   * Mark the given method as requiring an async transform.
   * Refer to documentation in the public API that forwards to this method in src/reflect/scala/reflect/api/Internals.scala
   */
  final def markForAsyncTransform(owner: Symbol, method: DefDef, awaitMethod: Symbol,
                                  config: Map[String, AnyRef]): DefDef = {
    val pos = owner.pos
    if (!settings.async)
      reporter.warning(pos, s"${settings.async.name} must be enabled for async transformation.")
    sourceFilesToTransform += pos.source
    val postAnfTransform = config.getOrElse("postAnfTransform", (x: Block) => x).asInstanceOf[Block => Block]
    val stateDiagram = config.getOrElse("stateDiagram", (sym: Symbol, tree: Tree) => None).asInstanceOf[(Symbol, Tree) => Option[String => Unit]]
    val allowExceptionsToPropagate = config.contains("allowExceptionsToPropagate")
    method.updateAttachment(new AsyncAttachment(awaitMethod, postAnfTransform, stateDiagram, allowExceptionsToPropagate))
    // Wrap in `{ expr: Any }` to force value class boxing before calling `completeSuccess`, see test/async/run/value-class.scala
    deriveDefDef(method) { rhs =>
      Block(Apply(gen.mkAttributedRef(definitions.Predef_locally), rhs :: Nil), Literal(Constant(())))
    }.updateAttachment(ChangeOwnerAttachment(owner))
  }

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  private def compileTimeOnlyPrefix: String = "[async] "

  /** Should refchecks defer reporting `@compileTimeOnly` errors for `sym` and instead let this phase issue the warning
   *  if they survive the async tranform? */
  private[scala] def deferCompileTimeOnlyError(sym: Symbol): Boolean = settings.async && {
    awaits.contains(sym) || {
      val msg = sym.compileTimeOnlyMessage.getOrElse("")
      val shouldDefer =
        msg.startsWith(compileTimeOnlyPrefix) || (sym.name == nme.await) && msg.contains("must be enclosed") && sym.owner.info.member(nme.async) != NoSymbol
      if (shouldDefer) awaits += sym
      shouldDefer
    }
  }

  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  final class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private lazy val liftableMap = new mutable.AnyRefMap[Symbol, (Symbol, List[Tree])]()

    override def transformUnit(unit: CompilationUnit): Unit = {
      if (settings.async) {
        // NoSourceFile can happen for, e.g., toolbox compilation; overestimate by always transforming them. See test/async/jvm/toolbox.scala
        val shouldTransform = unit.source == NoSourceFile || sourceFilesToTransform.contains(unit.source)
        if (shouldTransform) super.transformUnit(unit)
        if (awaits.exists(_.isInitialized)) {
          unit.body.foreach {
            case tree: RefTree if tree.symbol != null && awaits.contains(tree.symbol) =>
              val sym = tree.symbol
              val msg = sym.compileTimeOnlyMessage.getOrElse(s"`${sym.decodedName}` must be enclosed in an `async` block").stripPrefix(compileTimeOnlyPrefix)
              global.reporter.error(tree.pos, msg)
            case _ =>
          }
        }
      }
    }

    // Together, these transforms below target this tree shaps
    // {
    //    class $STATE_MACHINE extends ... {
    //      def $APPLY_METHOD(....) = {
    //      ...
    //      }.updateAttachment(AsyncAttachment(...))
    //    }
    // }
    //
    // The RHS of the method is transformed into a state machine with that transformation tailored by the
    // attached `FutureSystem`. Local val/var/def/class/object trees that are referred to from multiple states
    // are lifted into members of the enclosing class.
    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        case cd: ClassDef if liftableMap.contains(cd.symbol) =>
          val (applySym, liftedTrees) = liftableMap.remove(cd.symbol).get
          val liftedSyms = liftedTrees.iterator.map(_.symbol).toSet
          val cd1 = atOwner(cd.symbol) {
            deriveClassDef(cd)(impl => {
              deriveTemplate(impl)(liftedTrees ::: _)
            })
          }
          assert(localTyper.context.owner == cd.symbol.owner)
          val withFields = new UseFields(localTyper, cd.symbol, applySym, liftedSyms, NoSymbol).transform(cd1)
          withFields

        case dd: DefDef if dd.hasAttachment[AsyncAttachment] =>
          val asyncAttachment = dd.getAndRemoveAttachment[AsyncAttachment].get
          val asyncBody = (dd.rhs: @unchecked) match {
            case blk@Block(Apply(qual, body :: Nil) :: Nil, Literal(Constant(()))) => body
          }

          atOwner(dd, dd.symbol) {
            val trSym = dd.vparamss.head.last.symbol
            val selfSym = if (dd.symbol.owner.isTerm) dd.vparamss.head.head.symbol else NoSymbol
            val saved = currentTransformState
            currentTransformState = new AsyncTransformState(
              asyncAttachment.awaitSymbol,
              asyncAttachment.postAnfTransform,
              asyncAttachment.stateDiagram,
              asyncAttachment.allowExceptionsToPropagate,
              this,
              selfSym,
              trSym,
              asyncBody.tpe,
              asyncNames)
            try {
              val (newRhs, liftedTrees) = asyncTransform(asyncBody)
              liftableMap(currentTransformState.stateMachineClass) = (dd.symbol, liftedTrees)
              val liftedSyms = liftedTrees.iterator.map(_.symbol).toSet
              val withFields = new UseFields(localTyper, currentTransformState.stateMachineClass, dd.symbol, liftedSyms, selfSym).transform(newRhs)
              deriveDefDef(dd)(_ => withFields)
            } finally {
              currentTransformState = saved
            }
          }
        case tree =>
          tree
      }

    private def asyncTransform(asyncBody: Tree): (Tree, List[Tree]) = {
      val transformState = currentTransformState
      import transformState.applySym

      val asyncPos = asyncBody.pos

      // We mark whether each sub-tree of `asyncBody` that do or do not contain an await in thus pre-processing pass.
      // The ANF transform can then efficiently query this to selectively transform the tree.
      markContainsAwait(asyncBody)

      // Transform to A-normal form:
      //  - no await calls in qualifiers or arguments,
      //  - if/match only used in statement position.
      val anfTree: Block = transformState.postAnfTransform(new AnfTransformer(localTyper).apply(asyncBody))

      // The ANF transform re-parents some trees, so the previous traversal to mark ancestors of
      // await is no longer reliable. Clear previous results and run it again for use in the `buildAsyncBlock`.
      cleanupContainsAwaitAttachments(anfTree)
      markContainsAwait(anfTree)

      val asyncBlock = buildAsyncBlock(anfTree)

      val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

      // Null out lifted fields become unreachable at each state.
      val nullOut = true
      if (nullOut) {
        for ((state, (preNulls, postNulls)) <- fieldsToNullOut(asyncBlock.asyncStates, asyncBlock.asyncStates.last, liftedFields)) {
          val asyncState = asyncBlock.asyncStates.find(_.state == state).get
          if (asyncState.hasNonTerminalNextState)
            asyncState.insertNullAssignments(preNulls.iterator, postNulls.iterator)
        }
      }

      // Assemble the body of the apply method, which is dispactches on the current state id.
      val applyBody = atPos(asyncPos)(asyncBlock.onCompleteHandler)

      // Logging
      if ((settings.isDebug && shouldLogAtThisPhase))
        logDiagnostics(anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
      // Offer async frontends a change to produce the .dot diagram
      transformState.dotDiagram(applySym, asyncBody).foreach(f => f(asyncBlock.toDot))

      cleanupContainsAwaitAttachments(applyBody)

      (applyBody, liftedFields)
    }

    // Adjust the tree to:
    //   - lifted local variables are entered into the scope of the state machine class
    //   - references to them are rewritten as referencs to the fields.
    //   - the rhs of ValDefs that initialize such fields is turned into an assignment to the field
    private class UseFields(initLocalTyper: analyzer.Typer, stateMachineClass: Symbol,
                            applySym: Symbol, liftedSyms: Set[Symbol], selfSym: Symbol) extends explicitOuter.OuterPathTransformer(initLocalTyper) {
      private def fieldSel(tree: Tree) = {
        assert(currentOwner != NoSymbol)
        val outerOrThis =
          if (selfSym != NoSymbol)
            gen.mkAttributedIdent(selfSym)
          else if (stateMachineClass == currentClass)
            gen.mkAttributedThis(stateMachineClass)
          else {
            // These references need to be selected from an outer reference, because explicitouter
            // has already run we must perform this transform explicitly here.
            tree.symbol.makeNotPrivate(tree.symbol.owner)
            outerPath(outerValue, currentClass.outerClass, stateMachineClass)
          }
        atPos(tree.pos)(Select(outerOrThis.setType(stateMachineClass.tpe), tree.symbol).setType(tree.symbol.tpe))
      }
      override def transform(tree: Tree): Tree = tree match {
        case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) && currentOwner == applySym =>
          // Drop the lifted definitions from the apply method
          val rhs1 = transform(rhs.changeOwner(tree.symbol, currentOwner))
          deriveTree(rhs1, definitions.UnitTpe)(t => treeCopy.Assign(rhs1, fieldSel(tree), adapt(t, tree.symbol.tpe)))
        case _: DefTree if liftedSyms(tree.symbol) && currentOwner == applySym =>
          // Drop the lifted definitions from the apply method
          EmptyTree
        case md: MemberDef =>
          if (currentOwner == stateMachineClass) {
            if (liftedSyms(tree.symbol)) {
              stateMachineClass.info.decls.enter(md.symbol)
              super.transform(tree)
            } else if (md.symbol == applySym || md.symbol == stateMachineClass) {
              super.transform(tree)
            } else tree
          } else super.transform(tree)
        case Assign(i @ Ident(name), rhs) if liftedSyms(i.symbol) =>
          treeCopy.Assign(tree, fieldSel(i), adapt(transform(rhs), i.symbol.tpe))
        case Ident(name) if liftedSyms(tree.symbol) =>
          fieldSel(tree).setType(tree.tpe)
        case _: TypeTree =>
          tree
        case _ =>
          super.transform(tree)
      }

      // Use localTyper to adapt () to BoxedUnit in `val ifRes: Object; if (cond) "" else ()`
      private def adapt(tree: Tree, pt: Type): Tree = localTyper.typed(tree, pt)
    }

    private def logDiagnostics(anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
      val pos = currentTransformState.applySym.pos
      val location = try pos.source.path catch { case _: UnsupportedOperationException => pos.toString }
      inform(s"In file '$location':")
      inform(s"ANF transform expands to:\n $anfTree")
      states foreach (s => inform(s))
      inform("===== DOT =====")
      inform(block.toDot)
    }
  }
}
