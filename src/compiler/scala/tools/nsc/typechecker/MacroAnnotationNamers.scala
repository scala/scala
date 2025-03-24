/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.typechecker

import scala.tools.nsc.Reporting.WarningCategory

// imported from scalamacros/paradise
trait MacroAnnotationNamers { self: Analyzer =>
  import global._
  import scala.reflect.internal.Flags._
  import scala.reflect.internal.Mode._

  override def newNamer(context: Context): Namer = new MacroAnnotationNamer(context)

  class MacroAnnotationNamer(context: Context) extends Namer(context) {
    import NamerErrorGen._
    import typer.TyperErrorGen._

    override def standardEnterSym(tree: Tree): Context = {
      def dispatch() = {
        var returnContext = context
        tree match {
          case DocDef(_, mdef) =>
            enterSym(mdef)
          case tree @ Import(_, _) =>
            createAssignAndEnterSymbol(tree)
            finishSymbol(tree)
            returnContext = context.makeImportContext(tree)
          case tree: MemberDef =>
            createAssignAndEnterSymbol(tree)
            finishSymbol(tree)
          case _ =>
        }
        returnContext
      }
      tree.symbol match {
        case NoSymbol => try dispatch() catch typeErrorHandler(tree, context)
        case sym      => enterExistingSym(sym, tree)
      }
    }

    protected def createAssignAndEnterSymbol(tree: Tree, mask: Long = -1L): Symbol = {
      def coreCreateAssignAndEnterSymbol = {
        val sym = tree match {
          case PackageDef(pid, _) => createPackageSymbol(tree.pos, pid) // package symbols are entered elsewhere
          case imp: Import        => createImportSymbol(imp) // import symbols are dummies, no need to enter them anywhere
          case mdef: MemberDef    => enterInScope(setPrivateWithin(mdef, createMemberSymbol(mdef, mdef.name, mask)))
          case _                  => abort("Unexpected tree: " + tree)
        }
        if (isPastTyper) sym.name.toTermName match {
          case nme.IMPORT | nme.OUTER | nme.ANON_CLASS_NAME | nme.ANON_FUN_NAME | nme.CONSTRUCTOR =>
          case _ =>
            tree match {
              case _: DefDef => log("[+symbol] " + sym.debugLocationString)
              case _ =>
            }
        }
        tree.symbol = sym
        sym
      }
      def deriveSymbolFromSource(tree: Tree)(pf: PartialFunction[Tree, Symbol]): Symbol = {
        val sym = pf(tree)
        // can't do this in coreCreateAssignAndEnterSymbol
        // because then we won't get to update sources for redefinitions
        // this might be crucial when we have classfiles of the definition we're currently compiling
        attachSource(sym, tree)
        sym
      }
      deriveSymbolFromSource(tree) {
        case cdef @ ClassDef(mods, name, _, _) =>
          val existing = context.scope.lookup(name)
          val isRedefinition = (
                               existing.isType
                               && existing.isTopLevel
                               && context.scope == existing.owner.info.decls
                               && (
                                  currentRun.canRedefine(existing) ||
                                  isExpanded(existing)
                                  )
                               )
          val clazz: Symbol = {
            if (isRedefinition) {
              updatePosFlags(existing, cdef.pos, mods.flags)
              setPrivateWithin(cdef, existing)
              clearRenamedCaseAccessors(existing)
              cdef.symbol = existing
              existing
            }
            else coreCreateAssignAndEnterSymbol setFlag inConstructorFlag
          }
          if (clazz.isClass && clazz.isTopLevel) {
            if (clazz.sourceFile != null && clazz.sourceFile != contextFile)
              devWarning(s"Source file mismatch in $clazz: ${clazz.sourceFile} vs. $contextFile")

            clazz.associatedFile = contextFile
            if (clazz.sourceFile != null) {
              assert(currentRun.canRedefine(clazz) || clazz.sourceFile == currentRun.symSource(clazz), clazz.sourceFile)
              currentRun.symSource(clazz) = clazz.sourceFile
            }
            registerTopLevelSym(clazz)
            assert(clazz.name.toString.indexOf('(') < 0, clazz.name)  // )
          }
          clazz
        case mdef @ ModuleDef(mods, name, _) =>
          var m: Symbol = context.scope lookupModule name
          val moduleFlags = mods.flags | MODULE
          // TODO: inCurrentScope(m) check that's present in vanilla Namer is omitted here
          // this fixes SI-3772, but may break something else - I didn't have time to look into that
          if (m.isModule && !m.hasPackageFlag && (currentRun.canRedefine(m) || m.isSynthetic || isExpanded(m))) {
            // This code accounts for the way the package objects found in the classpath are opened up
            // early by the completer of the package itself. If the `packageobjects` phase then finds
            // the same package object in sources, we have to clean the slate and remove package object
            // members from the package class.
            //
            // TODO SI-4695 Pursue the approach in https://github.com/scala/scala/pull/2789 that avoids
            //      opening up the package object on the classpath at all if one exists in source.
            if (m.isPackageObject) {
              val packageScope = m.enclosingPackageClass.rawInfo.decls
              packageScope.filter(_.owner != m.enclosingPackageClass).toList.foreach(packageScope unlink _)
            }
            updatePosFlags(m, mdef.pos, moduleFlags)
            setPrivateWithin(mdef, m)
            m.moduleClass andAlso (setPrivateWithin(mdef, _))
            context.unit.synthetics -= m
            mdef.symbol = m
          }
          else {
            m = coreCreateAssignAndEnterSymbol
            m.moduleClass setFlag moduleClassFlags(moduleFlags)
            setPrivateWithin(mdef, m.moduleClass)
          }
          m.moduleClass setInfo namerOf(m).moduleClassTypeCompleter(mdef)
          if (m.isTopLevel && !m.hasPackageFlag) {
            m.moduleClass.associatedFile = contextFile
            currentRun.symSource(m) = m.moduleClass.sourceFile
            registerTopLevelSym(m)
          }
          m
        case _ =>
          coreCreateAssignAndEnterSymbol
      }
    }

    // reimplemented to integrate with weakEnsureCompanionObject
    override def standardEnsureCompanionObject(cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol = {
      val m = patchedCompanionSymbolOf(cdef.symbol, context)

      if (m != NoSymbol && currentRun.compiles(m) && !isWeak(m)) m
      else unmarkWeak(enterSyntheticSym(atPos(cdef.pos.focus)(creator(cdef))))
    }

    /** Does the same as `ensureCompanionObject`, but also makes sure that the returned symbol destroys itself
      *  if noone ends up using it (either by calling `ensureCompanionObject` or by `finishSymbol`).
      */
    // TODO: deduplicate
    protected def weakEnsureCompanionObject(cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol = {
      val m = patchedCompanionSymbolOf(cdef.symbol, context)
      if (m != NoSymbol && currentRun.compiles(m)) m
      else {
        val existsVal = context.tree.children.find {
          case ValDef(_, term, _, _) if cdef.getterName == term  => true
          case _ => false
        }
        if (existsVal.isDefined) NoSymbol else {
          val mdef = atPos(cdef.pos.focus)(creator(cdef))
          enterSym(mdef)
          markWeak(mdef.symbol)
        }
      }
    }

    protected def finishSymbol(tree: Tree): Unit = {
      // annotations on parameters expand together with their owners
      // therefore when we actually get to enter the parameters, we shouldn't even bother checking
      // TODO: we don't handle primary ctors that might get spuriously marked as maybe expandees because of primary paramss
      val aprioriNotExpandable = (context.tree, tree) match {
        case (ClassDef(_, _, _, _), TypeDef(_, _, _, _)) => true
        case (Template(_, _, _), ValDef(mods, _, _, _)) if mods.isParamAccessor => true
        // vparamss of primary ctors are entered in `enterValueParams`, which doesn't call us
        case (DefDef(_, _, _, _, _, _), TypeDef(_, _, _, _)) => true
        // vparamss of normal methods are also entered in `enterValueParams`, which doesn't call us
        case (TypeDef(_, _, _, _), TypeDef(_, _, _, _)) => true
        case _ => false
      }

      if (aprioriNotExpandable) finishSymbolNotExpandee(tree)
      else {
        treeInfo.getAnnotationZippers(tree) match {
          case Nil => finishSymbolNotExpandee(tree)
          case zippers => finishSymbolMaybeExpandee(tree, zippers)
        }

        // this will only show companions defined above ourselves
        // so when finishing `class C` in `{ class C; object C }`
        // we won't see `object C` in `companion` - we will see NoSymbol
        // that's the limitation of how namer works, but nevertheless it's not a problem for us
        // because if finishing `class C` doesn't set up the things, finishing `object C` will
        val sym = tree.symbol
        val companion = patchedCompanionSymbolOf(sym, context)

        tree match {
          // TODO: should we also support annotations on modules expanding companion classes?
          case tree @ ClassDef(_, _, _, _) if isMaybeExpandee(sym) =>
            val wasExpanded = isExpanded(companion)
            val m = weakEnsureCompanionObject(tree)
            finishSymbolMaybeExpandeeCompanion(attachedSource(m), m, sym)
            if (wasExpanded) markExpanded(m) // why is this necessary? see files/run/macro-annotation-recursive-class
          // TODO: in general, this first call to FSMEC usually only brings grief
          // can we get rid of it completely without having to sweep its results under the carpet?
          case tree @ ModuleDef(_, _, _) if isMaybeExpandee(companion) =>
            finishSymbolMaybeExpandeeCompanion(tree, sym, companion)
          case _ =>
        }
      }
    }

    protected def finishSymbolNotExpandee(tree: Tree): Unit = {
      val sym = tree.symbol
      def savingLock[T](op: => T): T = {
        val wasLocked = sym.hasFlag(LOCKED)
        val result = op
        if (wasLocked) sym.setFlag(LOCKED)
        result
      }

      savingLock {
        tree match {
          case tree @ PackageDef(_, _) =>
            newNamer(context.make(tree, sym.moduleClass, sym.info.decls)).enterSyms(tree.stats)
          case tree @ ClassDef(mods, name, tparams, impl) =>
            val primaryConstructorArity = treeInfo.firstConstructorArgs(impl.body).size
            // not entering
            tree.symbol setInfo completerOf(tree)

            if (mods.isCase) {
              val m = ensureCompanionObject(tree, caseModuleDef)
              m.moduleClass.updateAttachment(new ClassForCaseCompanionAttachment(tree))
            }
            val hasDefault = impl.body exists treeInfo.isConstructorWithDefault
            if (hasDefault) {
              val m = ensureCompanionObject(tree)
              m.updateAttachment(new ConstructorDefaultsAttachment(tree, null))
            }
            val owner = tree.symbol.owner
            if (settings.warnPackageObjectClasses && owner.isPackageObjectClass && !mods.isImplicit) {
              context.warning(tree.pos,
                               "it is not recommended to define classes/objects inside of package objects.\n" +
                               "If possible, define " + tree.symbol + " in " + owner.skipPackageObject + " instead.",
                              WarningCategory.LintPackageObjectClasses)
            }
            // Suggested location only.
            if (mods.isImplicit) {
                if (primaryConstructorArity == 1) {
                  log("enter implicit wrapper "+tree+", owner = "+owner)
                  enterImplicitWrapper(tree)
                }
                else reporter.error(tree.pos, "implicit classes must accept exactly one primary constructor parameter")
            }
            validateCompanionDefs(tree)
          case tree @ ModuleDef(_, _, _) =>
            unmarkWeak(sym)
            sym setInfo completerOf(tree)
            validateCompanionDefs(tree)
          case tree @ ValDef(_, _, _, _) =>
            val isScala = !context.unit.isJava
            if (isScala) {
              if (nme.isSetterName(tree.name)) ValOrVarWithSetterSuffixError(tree)
              if (tree.mods.isPrivateLocal && tree.mods.isCaseAccessor) PrivateThisCaseClassParameterError(tree)
            }
            if (isScala && deriveAccessors(tree)) {
              // when refactoring enterSym, I needed to decouple symbol creation and various syntheses
              // so that annotation expansion mechanism could be installed in-between of those
              // it went well except for one thing - ValDef symbol creation is very closely tied to syntheses
              // because depending on whether the ValDef is a val, var or a lazy val, different symbols need to be generated
              // since I didn't have much time (and, back then, much understanding), I just decided to create dummies
              // that live only to stand in as potential annottees and get destroyed if any sort of synthesis is necessary
              // TODO: this is obviously ugly and needs to be fixed
              context.scope.unlink(tree.symbol)
              tree.symbol setInfo NoType
              enterGetterSetter(tree)
            } else {
              tree.symbol setInfo completerOf(tree)
            }
            if (isEnumConstant(tree))
              tree.symbol setInfo ConstantType(Constant(tree.symbol))
          case tree @ DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
            if (mexists(tree.vparamss)(_.mods.hasDefault))
              enterDefaultGetters(tree.symbol, tree, tree.vparamss, tree.tparams)
            sym setInfo completerOf(tree)
          case tree @ DefDef(mods, name, tparams, _, _, _) =>
            if (mexists(tree.vparamss)(_.mods.hasDefault))
              enterDefaultGetters(tree.symbol, tree, tree.vparamss, tree.tparams)

            val completer =
              if (sym hasFlag SYNTHETIC) {
                if (name == nme.copy) copyMethodCompleter(tree)
                else if (sym hasFlag CASE) applyUnapplyMethodCompleter(tree, context)
                else completerOf(tree)
              } else completerOf(tree)
            sym setInfo completer
          case tree @ TypeDef(_, _, _, _) =>
            sym setInfo completerOf(tree)
          case tree @ Import(_, _) =>
            namerOf(tree.symbol) importTypeCompleter tree
          case x => throw new MatchError(x)
        }
      }
    }

    // we have several occasions when so called "maybe expandees" need special care
    // ("maybe expandees" = annotated members, which might or might not be annotated with a macro expansion)
    // 1) (when called by Symbol.info) trigger the MaybeExpandeeCompleter and then immediately recur into a fresh completer
    //    if we don't recur, we're doomed to fail, because there are only so many retries that Symbol.info can tolerate
    //    and this retry threshold is already fine-tuned to the current chain of completers, which makes MaybeExpandeeCompleter one too many
    // 2) (when called by expandMacroAnnotations from templateSig or typedBlock) in this situation noone needs us to fully complete
    //    the underlying symbol. just making sure that we don't have any annotations to expand is the least and the most we should do.
    //    if we're overeager like in mode #1, we might easily induce cyclic reference errors (like in tests/run/macro-annotations-packageobject)
    // 3) (when called by Symbol.typeParams) this one is different from Symbol.info, because it calls load, not complete
    //    from what I understand, this separation exists because it takes much less effort to figure out tparams rather than the full signature
    //    for example, vanilla completers assigned in namer are created with typeParams already known
    //    you can see for yourself in the distinction between monoTypeCompleter and PolyTypeCompleter
    //    therefore, just as with Symbol.info we need to trigger the MaybeExpandeeCompleter
    //    and then not forget to recur into the fresh completer's load, again because of the retry limit baked into Symbol.typeParams
    // 4) TODO: (when called by Symbol.unsafeTypeParams) figure out what's the deal with them
    //    existence of this method profoundly scares me, even though I never had a problem with it
    abstract class MaybeExpandeeCompleter(val tree: Tree) extends LockingTypeCompleter with FlagAssigningCompleter {
      def destroy(syms: Symbol*) = {
        for (sym <- syms) {
          context.unit.synthetics -= sym
          context.scope.unlink(sym)
          sym setInfo NoType
          sym.moduleClass setInfo NoType
          sym.removeAttachment[SymbolCompleterAttachment]
        }
      }

      def complete(sym: Symbol, onlyExpansions: Boolean) = {
        lockedCount += 1
        try completeImpl(sym, onlyExpansions)
        finally lockedCount -= 1
      }

      override def completeImpl(sym: Symbol): Unit = {
        completeImpl(sym, onlyExpansions = false)
      }

      def completeImpl(sym: Symbol, onlyExpansions: Boolean): Unit = {
        val thisCompleter = sym.rawInfo
        maybeExpand()
        assert(sym.rawInfo != thisCompleter, s"${sym.accurateKindString} ${sym.rawname}#${sym.id} with $kind")
        if (onlyExpansions) sym.rawInfo.completeOnlyExpansions(sym)
        else sym.rawInfo.complete(sym)
      }

      override def load(sym: Symbol): Unit = {
        this.completeOnlyExpansions(sym)
        sym.rawInfo.load(sym)
      }

      def maybeExpand(): Unit // TODO: should I also pass `sym` here?
    }

    abstract class MaybeExpandeeCompanionCompleter(tree: Tree) extends MaybeExpandeeCompleter(tree)

    private implicit class RichType(tpe: Type) {
      def completeOnlyExpansions(sym: Symbol) = tpe match {
        case mec: MacroAnnotationNamer#MaybeExpandeeCompleter => mec.complete(sym, onlyExpansions = true)
        case _ =>
      }
    }

    protected def finishSymbolMaybeExpandee(tree: Tree, annZippers: List[treeInfo.AnnotationZipper]): Unit = {
      val sym = tree.symbol
      unmarkWeak(sym)
      markMaybeExpandee(sym)
      sym.setInfo(new MaybeExpandeeCompleter(tree) {
        override def kind = s"maybeExpandeeCompleter for ${sym.accurateKindString} ${sym.rawname}#${sym.id}"
        override def maybeExpand(): Unit = {
          val companion = if (this.tree.isInstanceOf[ClassDef]) patchedCompanionSymbolOf(sym, context) else NoSymbol

          def maybeExpand(annotation: Tree, annottee: Tree, maybeExpandee: Tree): Option[List[Tree]] =
            if (context.macrosEnabled) { // TODO: when is this bit flipped -- can we pull this check out farther?
              val treeInfo.Applied(Select(New(tpt), nme.CONSTRUCTOR), _, _) = annotation: @unchecked
              val mann = probeMacroAnnotation(context, tpt)
              if (mann.isClass && mann.hasFlag(MACRO)) {
                assert(!currentRun.compiles(mann), mann)
                val annm = prepareAnnotationMacro(annotation, mann, sym, annottee, maybeExpandee)
                expandAnnotationMacro(this.tree, annm)
                // if we encounter an error, we just return None, so that other macro annotations can proceed
                // this is unlike macroExpand1 when any error in an expandee blocks expansions
                // there it's necessary in order not to exacerbate typer errors
                // but when manning we aren't in typer, so we don't have to do as macroExpand1 does
                // and also there's a good reason not to ban other macro annotations
                // if we do ban them, we might get spurious compilation errors from non-existent members that could've been generated
              } else None
            } else None

          annZippers.iterator.flatMap(annz => maybeExpand(annz.annotation, annz.annottee, annz.owner)).nextOption() match {
            case Some(expanded) =>
              // TODO: The workaround employed in https://github.com/scalamacros/paradise/issues/19
              // no longer works because of the REPL refactoring in 2.13.0-M2.
              // See https://github.com/scalamacros/paradise/issues/102 for discussion.
              //tellReplAboutExpansion(sym, companion, expanded)

              markExpanded(sym)
              markExpanded(companion)
              // expansion brings new trees, probably wildly different from current ones. what do we do?
              // the most robust thing would be to destroy ourselves (us and our companion), but we can't do that at top level
              // therefore at top level we don't destroy, but rather rely on enterSyms to redefine ourselves
              // however when nested we go all out
              // TODO: unlinking distorts the order of symbols in scope
              // note however that trees (calculated by expandMacroAnnotations) will be generated in correct order
              if (!sym.isTopLevel) destroy(sym, companion)
              enterSyms(expanded) // TODO: we can't reliably expand into imports, because they won't be accounted by definitions below us
            case None =>
              markNotExpandable(sym)
              finishSymbolNotExpandee(this.tree)
          }

          // take care of the companion if it's no longer needed
          // we can't do this in companion's completer, because that one isn't guaranteed to ever be called
          val expandedWithoutCompanion = isExpanded(sym) && attachedExpansion(companion).map(_.isEmpty).getOrElse(false)
          val companionHasReemerged = expandedWithoutCompanion && sym.isTopLevel && !isWeak(companion)
          val notExpandableWeakCompanion = isNotExpandable(sym) && isWeak(companion)
          if ((expandedWithoutCompanion && !companionHasReemerged) || notExpandableWeakCompanion) destroy(companion)
        }
      })
    }

    // how do we make sure that this completer falls back to the vanilla completer if the companion ends up not expanding?
    // well, if a module symbol has a maybeExpandee companion then the last two calls to its setInfo will be one of:
    //   * non-FSMEC completer for the module and then FSMEC => fallback should call native completer
    //   * FSMEC from enterSyntheticSym for a phantom module and then FSMEC again => fallback should do nothing
    // now it's easy to see that both are correctly handled here
    protected def finishSymbolMaybeExpandeeCompanion(tree: Tree, m: Symbol, c: Symbol): Unit = {
      val worthBackingUp = !m.rawInfo.isInstanceOf[MacroAnnotationNamer#MaybeExpandeeCompanionCompleter]
      if (worthBackingUp) backupCompleter(m)
      markMaybeExpandee(m)
      m.setInfo(new MaybeExpandeeCompanionCompleter(tree) {
        override def kind = s"maybeExpandeeCompanionCompleter for ${m.rawname}#${m.id}"
        override def maybeExpand(): Unit = {
          c.rawInfo.completeOnlyExpansions(c)
          // this is a very tricky part of annotation expansion
          // because now, after deferring to our companion's judgement for a while, we have to ourselves figure out:
          //   1) whether we should start completing on our own
          //   2) if we should do it on our own, then how exactly
          // 1 is easy. If our companion's expansion has destroyed us (or hasn't materialized us if we were weak)
          // then we no longer care and we silently go into oblivion. Otherwise, we should take care of ourselves.
          // 2 is hard, because we have two distinct situations to handle:
          //   2a) isExpanded(c) is true, which means that our companion has just expanded
          //   2b) isNotExpandable(c) is true, which means that our companion has just been deemed unexpandable
          // 2a is simple, because it means that we don't have to do anything, as we've either got destroyed
          // or we've got entered in `enterSyms(expanded)` that follows expansions.
          // 2b is tricky, because it means that we need to fall back to the most recent non-FSMEC completer.
          // The hardest part here is that we can't just get to the completer that was preceding `this` as m.rawInfo
          // (otherwise we run into issue #9, for more details see history of this change). Instead we need to track m's type history.
          val destroyedDuringExpansion = m.rawInfo == NoType
          val failedToMaterializeDuringExpansion = isWeak(m)
          val aliveAndKicking = !destroyedDuringExpansion && !failedToMaterializeDuringExpansion
          if (aliveAndKicking && isNotExpandable(c)) {
            if (worthBackingUp) restoreCompleter(m)
            val maybeExpandee = m.rawInfo.isInstanceOf[MacroAnnotationNamer#MaybeExpandeeCompleter]
            if (maybeExpandee) markMaybeExpandee(m) else markNotExpandable(m)
          }
        }
      })
    }

    // mostly copy/pasted and adapted from typedIdent
    // adaptations = ignore error reporting + ignore java + don't force symbols being compiled
    // the last requirement leads to us being imprecise in some situation wrt normal name resolution
    // but that's okay, since it's the only way for manns to remain modular and not to cripple normal annotations
    protected def probeMacroAnnotation(context: Context, tpt: Tree): Symbol = {
      // SAFE HELPERS (can't cause unnecessary completions)
      def reallyExists(sym: Symbol) = { if (newTyper(context).isStale(sym)) sym.setInfo(NoType); exists(sym) }
      def qualifies(sym: Symbol): Boolean = sym.hasRawInfo && reallyExists(sym)

      // UNSAFE HELPERS (need to guard against unnecessary completions)
      def canDefineMann(sym: Symbol): Boolean = !currentRun.compiles(sym)
      def exists(sym: Symbol) = if (canDefineMann(sym)) sym.exists else false
      def importedSymbol(imp: ImportInfo, name: Name): Symbol = { // TODO: be more precise in reproducing importSig and importedSymbol
      val impContext = context.enclosingContextChain.find(_.tree.symbol == imp.tree.symbol).get
        val sym = imp.tree.cached("importQualProbe", probeMacroAnnotation(impContext.outer, imp.tree.expr))
        val pre = if (reallyExists(sym) && isAccessible(impContext, sym)) sym.tpe else NoType
        var result: Symbol = NoSymbol
        var renamed = false
        var selectors = imp.tree.selectors
        def current = selectors.head
        while (selectors != Nil && result == NoSymbol) {
          if (current.introduces(name))
            result = nonLocalMember(pre, if (name.isTypeName) current.name.toTypeName else current.name)
          else if (selectors.head.name == name.toTermName)
            renamed = true
          else if (current.isWildcard && !renamed)
            result = nonLocalMember(pre, name)
          if (result == NoSymbol)
            selectors = selectors.tail
        }
        if (settings.warnUnusedImport && selectors.nonEmpty && result != NoSymbol && imp.pos != NoPosition) {
          val m_recordUsage = imp.getClass.getDeclaredMethods().find(_.getName == "recordUsage").get
          m_recordUsage.setAccessible(true)
          m_recordUsage.invoke(imp, current, result)
        }
        if (definitions isImportable result) result
        else NoSymbol
      }
      // def isAccessible(cx: Context, sym: Symbol) = if (canDefineMann(cx.owner)) cx.isAccessible(sym, cx.prefix, superAccess = false) else false
      def isAccessible(cx: Context, sym: Symbol) = true // TODO: sorry, it's 2am, and I can't figure this out
      def member(tpe: Type, name: Name) = if (canDefineMann(tpe.typeSymbol)) tpe.member(name) else NoSymbol
      def nonLocalMember(tpe: Type, name: Name) = if (canDefineMann(tpe.typeSymbol)) tpe.nonLocalMember(name) else NoSymbol

      if (tpt.hasSymbolField && tpt.symbol != NoSymbol) tpt.symbol
      else tpt match {
        case Ident(name) =>

          // STEP 1: RESOLVE THE NAME IN SCOPE
          var defSym: Symbol = NoSymbol
          var defEntry: ScopeEntry = null
          var cx = context
          while (defSym == NoSymbol && cx != NoContext && (cx.scope ne null)) {
            defEntry = cx.scope.lookupEntry(name)
            if ((defEntry ne null) && qualifies(defEntry.sym)) defSym = defEntry.sym
            else {
              cx = cx.enclClass
              val foundSym = member(cx.prefix, name) filter qualifies
              defSym = foundSym filter (isAccessible(cx, _))
              if (defSym == NoSymbol) cx = cx.outer
            }
          }
          if (defSym == NoSymbol && settings.exposeEmptyPackage.value) {
            defSym = rootMirror.EmptyPackageClass.info member name
          }

          // STEP 2: RESOLVE THE NAME IN IMPORTS
          val symDepth = if (defEntry eq null) cx.depth
                         else cx.depth - ({
                                            if (cx.scope ne null) cx.scope.nestingLevel
                                            else 0 // TODO: fix this in toolboxes, not hack around here
                                          } - defEntry.owner.nestingLevel)
          var impSym: Symbol = NoSymbol
          var imports = context.imports
          while (!reallyExists(impSym) && !imports.isEmpty && imports.head.depth > symDepth) {
            impSym = importedSymbol(imports.head, name)
            if (!exists(impSym)) imports = imports.tail
          }

          // FIXME: repl hack. somehow imports that come from repl are doubled
          // e.g. after `import $line7.$read.$iw.$iw.foo` you'll have another identical `import $line7.$read.$iw.$iw.foo`
          // this is a crude workaround for the issue
          imports match {
            case fst :: snd :: _ if exists(impSym) && fst == snd => imports = imports.tail
            case _ => // do nothing
          }

          // STEP 3: TRY TO RESOLVE AMBIGUITIES
          if (exists(defSym) && exists(impSym)) {
            if (defSym.isDefinedInPackage &&
                (!currentRun.compiles(defSym) ||
                 context.unit.exists && defSym.sourceFile != context.unit.source.file))
              defSym = NoSymbol
            else if (impSym.isError || impSym.name == nme.CONSTRUCTOR)
                   impSym = NoSymbol
          }
          if (!exists(defSym) && exists(impSym)) {
            var impSym1: Symbol = NoSymbol
            var imports1 = imports.tail
            while (!imports1.isEmpty &&
                   (!imports.head.isExplicitImport(name) ||
                    imports1.head.depth == imports.head.depth)) {
              impSym1 = importedSymbol(imports1.head, name)
              if (reallyExists(impSym1)) {
                if (imports1.head.isExplicitImport(name)) {
                  if (imports.head.isExplicitImport(name) ||
                      imports1.head.depth != imports.head.depth) return NoSymbol // was possibly fixable ambiguous import
                  impSym = impSym1
                  imports = imports1
                } else if (!imports.head.isExplicitImport(name) &&
                           imports1.head.depth == imports.head.depth) return NoSymbol // was possibly fixable ambiguous import
              }
              imports1 = imports1.tail
            }
          }

          // STEP 4: DEAL WITH WHAT WE HAVE
          if (exists(defSym) && !exists(impSym)) defSym
          else if (exists(defSym) && exists(impSym)) NoSymbol // was ambiguous import
          else if (!exists(defSym) && exists(impSym)) impSym
          else {
            val lastTry = rootMirror.missingHook(rootMirror.RootClass, name)
            if (lastTry != NoSymbol && isAccessible(context, lastTry)) lastTry
            else NoSymbol
          }
        case Select(qualtree, name) => // TODO: be more precise wrt typedSelect
          val qual = probeMacroAnnotation(context, qualtree)
          val sym = if (canDefineMann(qual)) member(qual.tpe, name) else NoSymbol
          if (reallyExists(sym) && isAccessible(context, sym)) sym else NoSymbol
        case AppliedTypeTree(tpt, _) => // https://github.com/scalamacros/paradise/issues/2: expand manns with type parameters
          probeMacroAnnotation(context, tpt)
        case _ =>
          NoSymbol
      }
    }

    // see https://github.com/scalamacros/paradise/issues/7
    // also see https://github.com/scalamacros/paradise/issues/64
    protected def patchedCompanionSymbolOf(original: Symbol, ctx: Context): Symbol = if (original == NoSymbol) NoSymbol else {
      val owner = original.owner
      // SI-7264 Force the info of owners from previous compilation runs.
      //         Doing this generally would trigger cycles; that's what we also
      //         use the lower-level scan through the current Context as a fall back.
      if (!currentRun.compiles(owner) &&
          // NOTE: the following three lines of code are added to work around #7
          !owner.enclosingTopLevelClass.isRefinementClass &&
          !owner.ownerChain.exists(_.isLocalDummy) &&
          owner.ownerChain.forall(!currentRun.compiles(_))) {
        owner.initialize
      }
      original.companionSymbol orElse {
        implicit class PatchedContext(ctx: Context) {
          trait PatchedLookupResult { def suchThat(criterion: Symbol => Boolean): Symbol }
          def patchedLookup(name: Name, expectedOwner: Symbol) = new PatchedLookupResult {
            override def suchThat(criterion: Symbol => Boolean): Symbol = {
              var res: Symbol = NoSymbol
              var ctx = PatchedContext.this.ctx
              while (res == NoSymbol && ctx.outer != ctx) {
                // NOTE: original implementation says `val s = ctx.scope lookup name`
                // but we can't use it, because Scope.lookup returns wrong results when the lookup is ambiguous
                // and that triggers https://github.com/scalamacros/paradise/issues/64
                val s = {
                  val lookupResult = ctx.scope.lookupAll(name).filter(criterion).toList
                  lookupResult match {
                    case Nil => NoSymbol
                    case List(unique) => unique
                    case _ => abort(s"unexpected multiple results for a companion symbol lookup for $original#{$original.id}")
                  }
                }
                if (s != NoSymbol && s.owner == expectedOwner)
                  res = s
                else
                  ctx = ctx.outer
              }
              res
            }
          }
        }
        ctx.patchedLookup(original.name.companionName, owner).suchThat(sym =>
                                                                         (original.isTerm || sym.hasModuleFlag) &&
                                                                         (sym isCoDefinedWith original)
                                                                      )
      }
    }

    protected def prepareAnnotationMacro(ann: Tree, mann: Symbol, sym: Symbol, annottee: Tree, expandee: Tree): Tree = {
      val companion = if (expandee.isInstanceOf[ClassDef]) patchedCompanionSymbolOf(sym, context) else NoSymbol
      val companionSource = if (!isWeak(companion)) attachedSource(companion) else EmptyTree
      val expandees = List(annottee, expandee, companionSource).distinct.filterNot(_.isEmpty)
      val safeExpandees = expandees.map(expandee => duplicateAndKeepPositions(expandee)).map(_.setSymbol(NoSymbol))
      val prefix = Select(ann, nme.macroTransform) setSymbol mann.info.member(nme.macroTransform) setPos ann.pos
      Apply(prefix, safeExpandees) setPos ann.pos
    }

    protected def expandAnnotationMacro(original: Tree, expandee: Tree): Option[List[Tree]] = {
      val sym = original.symbol
      val companion = if (original.isInstanceOf[ClassDef]) patchedCompanionSymbolOf(sym, context) else NoSymbol
      val wasWeak = isWeak(companion)
      val wasTransient = companion == NoSymbol || companion.isSynthetic
      def rollThroughImports(context: Context): Context = {
        if (context.isInstanceOf[ImportContext]) rollThroughImports(context.outer)
        else context
      }
      val typer = {
        // expanding at top level => allow the macro to see everything
        if (sym.isTopLevel) newTyper(context)
        // expanding at template level => only allow to see outside of the enclosing class
        // we have to skip two contexts:
        //  1) the Template context that hosts members
        //  2) the ImplDef context that hosts type params (and just them?)
        // upd. actually, i don't think we should skip the second context
        // that doesn't buy us absolutely anything wrt robustness
        else if (sym.owner.isClass) newTyper(rollThroughImports(context).outer)
             // expanding at block level => only allow to see outside of the block
        else newTyper(rollThroughImports(context).outer)
      }
      def onlyIfExpansionAllowed[T](expand: => Option[T]): Option[T] = {
        if (settings.Ymacroexpand.value == settings.MacroExpand.None) None
        else {
          val oldYmacroexpand = settings.Ymacroexpand.value
          try { settings.Ymacroexpand.value = settings.MacroExpand.Normal; expand }
          catch { case ex: Exception => settings.Ymacroexpand.value = oldYmacroexpand; throw ex }
        }
      }
      def expand(): Option[Tree] = (new DefMacroExpander(typer, expandee, NOmode, WildcardType) {
        override def onSuccess(expanded: Tree) = expanded
      })(expandee) match {
        case tree if tree.isErroneous => None
        case tree => Some(tree)
      }
      def extract(expanded: Tree): List[Tree] = expanded match {
        case Block(stats, Literal(Constant(()))) => stats // ugh
        case tree => List(tree)
      }
      def validate(expanded: List[Tree]): Option[List[Tree]] = {
        if (sym.owner.isPackageClass) {
          original match {
            case ClassDef(_, originalName, _, _) =>
              expanded match {
                case (expandedClass @ ClassDef(_, className, _, _)) :: Nil
                  if className == originalName && wasWeak =>
                  attachExpansion(sym, List(expandedClass))
                  attachExpansion(companion, Nil)
                  Some(expanded)
                case (expandedCompanion @ ModuleDef(_, moduleName, _)) :: (expandedClass @ ClassDef(_, className, _, _)) :: Nil
                  if className == originalName && moduleName == originalName.toTermName =>
                  attachExpansion(sym, if (wasWeak) List(expandedClass, expandedCompanion) else List(expandedClass))
                  attachExpansion(companion, List(expandedCompanion))
                  Some(expanded)
                case (expandedClass @ ClassDef(_, className, _, _)) :: (expandedCompanion @ ModuleDef(_, moduleName, _)) :: Nil
                  if className == originalName && moduleName == originalName.toTermName =>
                  attachExpansion(sym, if (wasWeak) List(expandedClass, expandedCompanion) else List(expandedClass))
                  attachExpansion(companion, List(expandedCompanion))
                  Some(expanded)
                case _ =>
                  if (wasWeak) MacroAnnotationTopLevelClassWithoutCompanionBadExpansion(expandee)
                  else MacroAnnotationTopLevelClassWithCompanionBadExpansion(expandee)
                  None
              }
            case ModuleDef(_, originalName, _) =>
              expanded match {
                case (expandedModule @ ModuleDef(_, expandedName, _)) :: Nil if expandedName == originalName =>
                  attachExpansion(sym, List(expandedModule))
                  Some(expanded)
                case _ =>
                  MacroAnnotationTopLevelModuleBadExpansion(expandee)
                  None
              }
            case x => throw new MatchError(x)
          }
        } else {
          if (wasTransient) {
            attachExpansion(sym, expanded)
            attachExpansion(companion, Nil)
          } else {
            def companionRelated(tree: Tree) = tree.isInstanceOf[ModuleDef] && tree.asInstanceOf[ModuleDef].name == companion.name
            val (forCompanion, forSym) = expanded.partition(companionRelated)
            attachExpansion(sym, forSym)
            attachExpansion(companion, forCompanion)
          }
          Some(expanded)
        }
      }
      for {
        lowlevelExpansion <- onlyIfExpansionAllowed(expand())
        expansion <- Some(extract(lowlevelExpansion))
        duplicated = expansion.map(duplicateAndKeepPositions)
        validatedExpansion <- validate(duplicated)
      } yield validatedExpansion
    }

    override def expandMacroAnnotations(stats: List[Tree]): List[Tree] = {
      def mightNeedTransform(stat: Tree): Boolean = stat match {
        case stat: DocDef => mightNeedTransform(stat.definition)
        case stat: MemberDef => isMaybeExpandee(stat.symbol) || hasAttachedExpansion(stat.symbol)
        case _ => false
      }
      def rewrapAfterTransform(stat: Tree, transformed: List[Tree]): List[Tree] = (stat, transformed) match {
        case (stat @ DocDef(comment, _), List(transformed: MemberDef)) => List(treeCopy.DocDef(stat, comment, transformed))
        case (DocDef(_, _), List(transformed: DocDef)) => List(transformed)
        case (_, Nil | List(_: MemberDef)) => transformed
        case (_, unexpected) => unexpected // NOTE: who knows how people are already using macro annotations, so it's scary to fail here
      }
      if (phase.id > currentRun.typerPhase.id || !stats.exists(mightNeedTransform)) stats
      else stats.flatMap { stat =>
        if (mightNeedTransform(stat)) {
          val sym = stat.symbol
          assert(sym != NoSymbol, (sym, stat))
          if (isMaybeExpandee(sym)) {
            def assert(what: Boolean) = Predef.assert(what, s"${sym.accurateKindString} ${sym.rawname}#${sym.id} with ${sym.rawInfo.kind}")
            assert(sym.rawInfo.isInstanceOf[MacroAnnotationNamer#MaybeExpandeeCompleter])
            sym.rawInfo.completeOnlyExpansions(sym)
            assert(!sym.rawInfo.isInstanceOf[MacroAnnotationNamer#MaybeExpandeeCompleter])
          }
          val derivedTrees = attachedExpansion(sym).getOrElse(List(stat))
          val (me, others) = derivedTrees.partition(_.symbol == sym)
          rewrapAfterTransform(stat, me) ++ expandMacroAnnotations(others)
        } else {
          List(stat)
        }
      }
    }
  }
}
