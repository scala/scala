package scala.reflect.reify
package phases

import scala.tools.nsc.symtab.Flags._

trait Reshape {
  self: Reifier =>

  import global._
  import definitions._
  import treeInfo.Unapplied
  private val runDefinitions = currentRun.runDefinitions
  import runDefinitions._

  /**
   *  Rolls back certain changes that were introduced during typechecking of the reifee.
   *
   *  These include:
   *    * Undoing macro expansions
   *    * Replacing type trees with TypeTree(tpe)
   *    * Reassembling CompoundTypeTrees into reifiable form
   *    * Transforming Modifiers.annotations into Symbol.annotations
   *    * Transforming Annotated annotations into AnnotatedType annotations
   *    * Transforming Annotated(annot, expr) into Typed(expr, TypeTree(Annotated(annot, _))
   *    * Non-idempotencies of the typechecker: https://issues.scala-lang.org/browse/SI-5464
   */
  val reshape = new Transformer {
    var currentSymbol: Symbol = NoSymbol

    override def transform(tree0: Tree) = {
      val tree = undoMacroExpansion(tree0)
      currentSymbol = tree.symbol

      val preTyper = tree match {
        case tree if tree.isErroneous =>
          tree
        case tt @ TypeTree() =>
          toPreTyperTypeTree(tt)
        case ctt @ CompoundTypeTree(_) =>
          toPreTyperCompoundTypeTree(ctt)
        case toa @ TypedOrAnnotated(_) =>
          toPreTyperTypedOrAnnotated(toa)
        case ta @ TypeApply(_, _) if isCrossStageTypeBearer(ta) =>
          if (reifyDebug) println("cross-stage type bearer, retaining: " + tree)
          ta
        case ta @ TypeApply(hk, ts) =>
          val discard = ts collect { case tt: TypeTree => tt } exists isDiscarded
          if (reifyDebug && discard) println("discarding TypeApply: " + tree)
          if (discard) hk else ta
        case classDef @ ClassDef(mods, name, params, impl) =>
          val Template(parents, self, body) = impl
          var body1 = trimAccessors(classDef, body)
          body1 = trimSyntheticCaseClassMembers(classDef, body1)
          val impl1 = Template(parents, self, body1).copyAttrs(impl)
          ClassDef(mods, name, params, impl1).copyAttrs(classDef)
        case moduledef @ ModuleDef(mods, name, impl) =>
          val Template(parents, self, body) = impl
          var body1 = trimAccessors(moduledef, body)
          body1 = trimSyntheticCaseClassMembers(moduledef, body1)
          val impl1 = Template(parents, self, body1).copyAttrs(impl)
          ModuleDef(mods, name, impl1).copyAttrs(moduledef)
        case template @ Template(parents, self, body) =>
          val discardedParents = parents collect { case tt: TypeTree => tt } filter isDiscarded
          if (reifyDebug && discardedParents.length > 0) println("discarding parents in Template: " + discardedParents.mkString(", "))
          val parents1 = parents diff discardedParents
          val body1 = trimSyntheticCaseClassCompanions(body)
          Template(parents1, self, body1).copyAttrs(template)
        case block @ Block(stats, expr) =>
          val stats1 = trimSyntheticCaseClassCompanions(stats)
          Block(stats1, expr).copyAttrs(block)
        case unapply @ UnApply(Unapplied(Select(fun, nme.unapply | nme.unapplySeq)), args) =>
          if (reifyDebug) println("unapplying unapply: " + tree)
          Apply(fun, args).copyAttrs(unapply)
        case _ =>
          tree
      }

      super.transform(preTyper)
    }

    private def undoMacroExpansion(tree: Tree): Tree =
      tree.attachments.get[analyzer.MacroExpansionAttachment] match {
        case Some(analyzer.MacroExpansionAttachment(original, _)) =>
          def mkImplicitly(tp: Type) = atPos(tree.pos)(
            gen.mkNullaryCall(Predef_implicitly, List(tp))
          )
          val sym = original.symbol
          original match {
            // this hack is necessary until I fix implicit macros
            // so far tag materialization is implemented by sneaky macros hidden in scala-compiler.jar
            // hence we cannot reify references to them, because noone will be able to see them later
            // when implicit macros are fixed, these sneaky macros will move to corresponding companion objects
            // of, say, ClassTag or TypeTag
            case Apply(TypeApply(_, List(tt)), _) if sym == materializeClassTag            => mkImplicitly(appliedType(ClassTagClass, tt.tpe))
            case Apply(TypeApply(_, List(tt)), List(pre)) if sym == materializeWeakTypeTag => mkImplicitly(typeRef(pre.tpe, WeakTypeTagClass, List(tt.tpe)))
            case Apply(TypeApply(_, List(tt)), List(pre)) if sym == materializeTypeTag     => mkImplicitly(typeRef(pre.tpe, TypeTagClass, List(tt.tpe)))
            case _                                                                         => original
          }
        case _ => tree
      }

    override def transformModifiers(mods: Modifiers) = {
      val mods1 = toPreTyperModifiers(mods, currentSymbol)
      super.transformModifiers(mods1)
    }

    private def toPreTyperModifiers(mods: Modifiers, sym: Symbol) = {
      if (!sym.annotations.isEmpty) {
        val postTyper = sym.annotations filter (_.original != EmptyTree)
        if (reifyDebug && !postTyper.isEmpty) println("reify symbol annotations for: " + sym)
        if (reifyDebug && !postTyper.isEmpty) println("originals are: " + sym.annotations)
        val preTyper = postTyper map toPreTyperAnnotation
        mods.withAnnotations(preTyper)
      } else {
        mods
      }
    }

    /** Restore pre-typer representation of a type.
     *
     *  NB: This is the trickiest part of reification!
     *
     *  In most cases, we're perfectly fine to reify a Type itself (see `reifyType`).
     *  However if the type involves a symbol declared inside the quasiquote (i.e. registered in `boundSyms`),
     *  then we cannot reify it, or otherwise subsequent reflective compilation will fail.
     *
     *  Why will it fail? Because reified deftrees (e.g. ClassDef(...)) will generate fresh symbols during that compilation,
     *  so naively reified symbols will become out of sync, which brings really funny compilation errors and/or crashes, e.g.:
     *  https://issues.scala-lang.org/browse/SI-5230
     *
     *  To deal with this unpleasant fact, we need to fall back from types to equivalent trees (after all, parser trees don't contain any types, just trees, so it should be possible).
     *  Luckily, these original trees get preserved for us in the `original` field when Trees get transformed into TypeTrees.
     *  And if an original of a type tree is empty, we can safely assume that this type is non-essential (e.g. was inferred/generated by the compiler).
     *  In that case the type can be omitted (e.g. reified as an empty TypeTree), since it will be inferred again later on.
     *
     *  An important property of the original is that it isn't just a pre-typer tree.
     *  It's actually kind of a post-typer tree with symbols assigned to its Idents (e.g. Ident("List") will contain a symbol that points to immutable.this.List).
     *  This is very important, since subsequent reflective compilation won't have to resolve these symbols.
     *  In general case, such resolution cannot be performed, since reification doesn't preserve lexical context,
     *  which means that reflective compilation won't be aware of, say, imports that were provided when the reifee has been compiled.
     *
     *  This workaround worked surprisingly well and allowed me to fix several important reification bugs, until the abstraction has leaked.
     *  Suddenly I found out that in certain contexts original trees do not contain symbols, but are just parser trees.
     *  To the moment I know only one such situation: typedAnnotations does not typecheck the annotation in-place, but rather creates new trees and typechecks them, so the original remains symless.
     *  Thus we apply a workaround for that in typedAnnotated. I hope this will be the only workaround in this department.
     *  upd. There are also problems with CompoundTypeTrees. I had to use attachments to retain necessary information.
     *
     *  upd. Recently I went ahead and started using original for all TypeTrees, regardless of whether they refer to local symbols or not.
     *  As a result, `reifyType` is never called directly by tree reification (and, wow, it seems to work great!).
     *  The only usage of `reifyType` now is for servicing typetags, however, I have some ideas how to get rid of that as well.
     */
    private def isDiscarded(tt: TypeTree) = tt.original == null
    private def toPreTyperTypeTree(tt: TypeTree): Tree = {
      if (!isDiscarded(tt)) {
        // here we rely on the fact that the originals that reach this point
        // have all necessary symbols attached to them (i.e. that they can be recompiled in any lexical context)
        // if this assumption fails, please, don't be quick to add postprocessing here (like I did before)
        // but rather try to fix this in Typer, so that it produces quality originals (like it's done for typedAnnotated)
        if (reifyDebug) println("TypeTree, essential: %s (%s)".format(tt.tpe, tt.tpe.kind))
        if (reifyDebug) println("verdict: rolled back to original %s".format(tt.original.toString.replaceAll("\\s+", " ")))
        transform(tt.original)
      } else {
        // type is deemed to be non-essential
        // erase it and hope that subsequent reflective compilation will be able to recreate it again
        if (reifyDebug) println("TypeTree, non-essential: %s (%s)".format(tt.tpe, tt.tpe.kind))
        if (reifyDebug) println("verdict: discarded")
        TypeTree()
      }
    }

    private def toPreTyperCompoundTypeTree(ctt: CompoundTypeTree): Tree = {
      val CompoundTypeTree(tmpl @ Template(parents, self, stats)) = ctt
      if (stats.nonEmpty) CannotReifyCompoundTypeTreeWithNonEmptyBody(ctt)
      assert(self eq noSelfType, self)
      val att = tmpl.attachments.get[CompoundTypeTreeOriginalAttachment]
      val CompoundTypeTreeOriginalAttachment(parents1, stats1) = att.getOrElse(CompoundTypeTreeOriginalAttachment(parents, stats))
      CompoundTypeTree(Template(parents1, self, stats1))
    }

    private def toPreTyperTypedOrAnnotated(tree: Tree): Tree = tree match {
      case ty @ Typed(expr1, tpt) =>
        if (reifyDebug) println("reify typed: " + tree)
        val original = tpt match {
          case tt @ TypeTree() => tt.original
          case tpt => tpt
        }
        val annotatedArg = {
          def loop(tree: Tree): Tree = tree match {
            case annotated1 @ Annotated(ann, annotated2 @ Annotated(_, _)) => loop(annotated2)
            case annotated1 @ Annotated(ann, arg) => arg
            case _ => EmptyTree
          }

          loop(original)
        }
        if (annotatedArg != EmptyTree) {
          if (annotatedArg.isType) {
            if (reifyDebug) println("verdict: was an annotated type, reify as usual")
            ty
          } else {
            if (reifyDebug) println("verdict: was an annotated value, equivalent is " + original)
            toPreTyperTypedOrAnnotated(original)
          }
        } else {
          if (reifyDebug) println("verdict: wasn't annotated, reify as usual")
          ty
        }
      case at @ Annotated(annot, arg) =>
        if (reifyDebug) println("reify type annotations for: " + tree)
        assert(at.tpe.isInstanceOf[AnnotatedType], "%s (%s)".format(at.tpe, at.tpe.kind))
        val annot1 = toPreTyperAnnotation(at.tpe.asInstanceOf[AnnotatedType].annotations(0))
        if (reifyDebug) println("originals are: " + annot1)
        Annotated(annot1, arg).copyAttrs(at)
    }

    /** Restore pre-typer representation of an annotation.
     *  The trick here is to retain the symbols that have been populated during typechecking of the annotation.
     *  If we do not do that, subsequent reflective compilation will fail.
     */
    private def toPreTyperAnnotation(ann: AnnotationInfo): Tree = {
      val args = if (ann.assocs.isEmpty) {
        ann.args
      } else {
        def toScalaAnnotation(jann: ClassfileAnnotArg): Tree = (jann: @unchecked) match {
          case LiteralAnnotArg(const) => Literal(const)
          case ArrayAnnotArg(arr)     => Apply(Ident(definitions.ArrayModule), arr.toList map toScalaAnnotation)
          case NestedAnnotArg(ann)    => toPreTyperAnnotation(ann)
        }

        ann.assocs map { case (nme, arg) => AssignOrNamedArg(Ident(nme), toScalaAnnotation(arg)) }
      }

      def extractOriginal: PartialFunction[Tree, Tree] = { case Apply(Select(New(tpt), _), _) => tpt }
      assert(extractOriginal.isDefinedAt(ann.original), showRaw(ann.original))
      New(TypeTree(ann.atp) setOriginal extractOriginal(ann.original), List(args))
    }

    private def toPreTyperLazyVal(ddef: DefDef): ValDef = {
      def extractRhs(rhs: Tree) = rhs match {
        case Block(Assign(lhs, rhs)::Nil, _) if lhs.symbol.isLazy => rhs
        case _ => rhs // unit or trait case
      }
      val DefDef(mods0, name0, _, _, tpt0, rhs0) = ddef
      val name1 = name0.dropLocal
      val Modifiers(flags0, privateWithin0, annotations0) = mods0
      val flags1 = (flags0 & GetterFlags) & ~(STABLE | ACCESSOR | METHOD)
      val mods1 = Modifiers(flags1, privateWithin0, annotations0) setPositions mods0.positions
      val mods2 = toPreTyperModifiers(mods1, ddef.symbol)
      ValDef(mods2, name1, tpt0, extractRhs(rhs0))
    }

    private def trimAccessors(deff: Tree, stats: List[Tree]): List[Tree] = {
      val symdefs = (stats collect { case vodef: ValOrDefDef => vodef } map (vodeff => vodeff.symbol -> vodeff)).toMap
      val accessors = scala.collection.mutable.Map[ValDef, List[DefDef]]()
      stats collect { case ddef: DefDef => ddef } foreach (defdef => {
        val valdef = symdefs get defdef.symbol.accessedOrSelf collect { case vdef: ValDef => vdef } getOrElse null
        if (valdef != null) accessors(valdef) = accessors.getOrElse(valdef, Nil) :+ defdef

        def detectBeanAccessors(prefix: String): Unit = {
          if (defdef.name.startsWith(prefix)) {
            val name = defdef.name.toString.substring(prefix.length)
            def uncapitalize(s: String) = if (s.length == 0) "" else { val chars = s.toCharArray; chars(0) = chars(0).toLower; new String(chars) }
            def findValDef(name: String) = symdefs.values collectFirst {
              case vdef: ValDef if vdef.name.dropLocal string_== name => vdef
            }
            val valdef = findValDef(name).orElse(findValDef(uncapitalize(name))).orNull
            if (valdef != null) accessors(valdef) = accessors.getOrElse(valdef, Nil) :+ defdef
          }
        }
        detectBeanAccessors("get")
        detectBeanAccessors("set")
        detectBeanAccessors("is")
      })

      val stats1 = stats flatMap {
        case vdef @ ValDef(mods, name, tpt, rhs) if !mods.isLazy =>
          val mods1 = if (accessors.contains(vdef)) {
            val ddef = accessors(vdef)(0) // any accessor will do
            val Modifiers(flags, _, annotations) = mods
            var flags1 = flags & ~LOCAL
            if (!ddef.symbol.isPrivate) flags1 = flags1 & ~PRIVATE
            val privateWithin1 = ddef.mods.privateWithin
            val annotations1 = accessors(vdef).foldLeft(annotations)((curr, acc) => curr ++ (acc.symbol.annotations map toPreTyperAnnotation))
            Modifiers(flags1, privateWithin1, annotations1) setPositions mods.positions
          } else {
            mods
          }
          val mods2 = toPreTyperModifiers(mods1, vdef.symbol)
          val name1 = name.dropLocal
          val vdef1 = ValDef(mods2, name1.toTermName, tpt, rhs)
          if (reifyDebug) println("resetting visibility of field: %s => %s".format(vdef, vdef1))
          Some(vdef1) // no copyAttrs here, because new ValDef and old symbols are now out of sync
        case ddef: DefDef if !ddef.mods.isLazy =>
          // lazy val accessors are removed in reshapeLazyVals
          // as they are needed to recreate lazy vals
          if (accessors.values.exists(_.contains(ddef))) {
            if (reifyDebug) println("discarding accessor method: " + ddef)
            None
          } else {
            Some(ddef)
          }
        case tree =>
          Some(tree)
      }

      stats1
    }

    private def trimSyntheticCaseClassMembers(deff: Tree, stats: List[Tree]): List[Tree] =
      stats filterNot (memberDef => memberDef.isDef && {
        val isSynthetic = memberDef.symbol.isSynthetic
        // this doesn't work for local classes, e.g. for ones that are top-level to a quasiquote (see comments to companionClass)
        // that's why I replace the check with an assumption that all synthetic members are, in fact, generated of case classes
        // val isCaseMember = deff.symbol.isCaseClass || deff.symbol.companionClass.isCaseClass
        val isCaseMember = true
        if (isSynthetic && isCaseMember && reifyDebug) println("discarding case class synthetic def: " + memberDef)
        isSynthetic && isCaseMember
      })

    private def trimSyntheticCaseClassCompanions(stats: List[Tree]): List[Tree] =
      stats diff (stats collect { case moddef: ModuleDef => moddef } filter (moddef => {
        val isSynthetic = moddef.symbol.isSynthetic
        // this doesn't work for local classes, e.g. for ones that are top-level to a quasiquote (see comments to companionClass)
        // that's why I replace the check with an assumption that all synthetic modules are, in fact, companions of case classes
        // val isCaseCompanion = moddef.symbol.companionClass.isCaseClass
        val isCaseCompanion = true
        if (isSynthetic && isCaseCompanion && reifyDebug) println("discarding synthetic case class companion: " + moddef)
        isSynthetic && isCaseCompanion
      }))
  }
}
