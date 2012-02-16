/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.tools.nsc
package ast

import symtab._
import Flags._
import scala.reflect.api.Modifier._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.FreshNameCreator
import scala.runtime.ScalaRunTime.{ isAnyVal, isTuple }

/** Given a tree or type, generate a tree that when executed at runtime produces the original tree or type.
 *  See more info in the comments to `reify' in scala.reflect.macro.Context.
 *
 *  @author Martin Odersky
 *  @version 2.10
 */
trait Reifiers { self: Global =>

  def reify(tree: Tree): Tree = {
    class Reifier {
      import definitions._
      import Reifier._

      final val scalaPrefix = "scala."
      final val localPrefix = "$local"
      final val memoizerName = "$memo"

      val reifyDebug = settings.Yreifydebug.value

      private val reifiableSyms = mutable.ArrayBuffer[Symbol]() // the symbols that are reified with the tree
      private val symIndex = mutable.HashMap[Symbol, Int]() // the index of a reifiable symbol in `reifiableSyms`
      private var boundSyms = Set[Symbol]() // set of all symbols that are bound in tree to be reified

      private def definedInLiftedCode(tpe: Type) =
        tpe exists (tp => boundSyms contains tp.typeSymbol)

      private def definedInLiftedCode(sym: Symbol) =
        boundSyms contains sym

      /**
       * Generate tree of the form
       *
       *    { val $mr = scala.reflect.runtime.Mirror
       *      $local1 = new TypeSymbol(owner1, NoPosition, name1)
       *      ...
       *      $localN = new TermSymbol(ownerN, NoPositiion, nameN)
       *      $local1.setInfo(tpe1)
       *      ...
       *      $localN.setInfo(tpeN)
       *      $localN.setAnnotations(annotsN)
       *      rtree
       *    }
       *
       *  where
       *
       *   - `$localI` are free type symbols in the environment, as well as local symbols
       *      of refinement types.
       *   - `tpeI` are the info's of `symI`
       *   - `rtree` is code that generates `data` at runtime, maintaining all attributes.
       *   - `data` is typically a tree or a type.
       */
      def reifyTopLevel(data: Any): Tree = {
        val rtree = reify(data)
        Block(mirrorAlias :: reifySymbolTableSetup, rtree)
      }

      private def isLocatable(sym: Symbol) =
        sym.isPackageClass || sym.owner.isClass || sym.isTypeParameter && sym.paramPos >= 0

      private def registerReifiableSymbol(sym: Symbol): Unit =
        if (!(symIndex contains sym)) {
          sym.owner.ownersIterator find (x => !isLocatable(x)) foreach registerReifiableSymbol
          symIndex(sym) = reifiableSyms.length
          reifiableSyms += sym
        }

      // helper methods

      private def localName(sym: Symbol): TermName =
        newTermName(localPrefix + symIndex(sym))

      private def call(fname: String, args: Tree*): Tree =
        Apply(termPath(fname), args.toList)

      private def mirrorSelect(name: String): Tree =
        termPath(nme.MIRROR_PREFIX + name)

      private def mirrorCall(name: TermName, args: Tree*): Tree =
        call("" + (nme.MIRROR_PREFIX append name), args: _*)

      private def mirrorCall(name: String, args: Tree*): Tree =
        call(nme.MIRROR_PREFIX + name, args: _*)

      private def mirrorFactoryCall(value: Product, args: Tree*): Tree =
        mirrorFactoryCall(value.productPrefix, args: _*)

      private def mirrorFactoryCall(prefix: String, args: Tree*): Tree =
        mirrorCall(prefix, args: _*)

      private def scalaFactoryCall(name: String, args: Tree*): Tree =
        call(scalaPrefix + name + ".apply", args: _*)

      private def mkList(args: List[Tree]): Tree =
        scalaFactoryCall("collection.immutable.List", args: _*)

      private def reifyModifiers(m: Modifiers) =
        mirrorCall("modifiersFromInternalFlags", reify(m.flags), reify(m.privateWithin), reify(m.annotations))

      private def reifyAggregate(name: String, args: Any*) =
        scalaFactoryCall(name, (args map reify).toList: _*)

      /**
       * Reify a list
       */
      private def reifyList(xs: List[Any]): Tree =
        mkList(xs map reify)

      /**
       * Reify an array
       */
      private def reifyArray(xs: Array[_]): Tree =
        // @xeno.by: doesn't work for Array(LiteralAnnotArg(...))
        // because we cannot generate manifests for path-dependent types
        scalaFactoryCall(nme.Array, xs map reify: _*)

      /** Reify a name */
      private def reifyName(name: Name) =
        mirrorCall(if (name.isTypeName) "newTypeName" else "newTermName", Literal(Constant(name.toString)))

      private def isFree(sym: Symbol) =
        !(symIndex contains sym)

      /**
       * Reify a reference to a symbol
       */
      private def reifySymRef(sym: Symbol): Tree = {
        symIndex get sym match {
          case Some(idx) =>
            Ident(localName(sym))
          case None =>
            if (sym == NoSymbol)
              mirrorSelect("NoSymbol")
            else if (sym == RootPackage)
              mirrorSelect("definitions.RootPackage")
            else if (sym == RootClass)
              mirrorSelect("definitions.RootClass")
            else if (sym == EmptyPackage)
              mirrorSelect("definitions.EmptyPackage")
            else if (sym.isModuleClass)
              Select(reifySymRef(sym.sourceModule), "moduleClass")
            else if (sym.isStatic && sym.isClass)
              mirrorCall("staticClass", reify(sym.fullName))
            else if (sym.isStatic && sym.isModule)
              mirrorCall("staticModule", reify(sym.fullName))
            else if (isLocatable(sym))
              if (sym.isTypeParameter)
                mirrorCall("selectParam", reify(sym.owner), reify(sym.paramPos))
              else {
                if (reifyDebug) println("locatable: " + sym + " " + sym.isPackageClass + " " + sym.owner + " " + sym.isTypeParameter)
                val rowner = reify(sym.owner)
                val rname = reify(sym.name.toString)
                if (sym.isType)
                  mirrorCall("selectType", rowner, rname)
                else if (sym.isMethod && sym.owner.isClass && sym.owner.info.decl(sym.name).isOverloaded) {
                  val index = sym.owner.info.decl(sym.name).alternatives indexOf sym
                  assert(index >= 0, sym)
                  mirrorCall("selectOverloadedMethod", rowner, rname, reify(index))
                } else
                  mirrorCall("selectTerm", rowner, rname)
              }
            else {
              if (sym.isTerm) {
                if (reifyDebug) println("Free: " + sym)
                val symtpe = lambdaLift.boxIfCaptured(sym, sym.tpe, erasedTypes = false)
                def markIfCaptured(arg: Ident): Tree =
                  if (sym.isCapturedVariable) referenceCapturedVariable(arg) else arg
                mirrorCall("newFreeVar", reify(sym.name.toString), reify(symtpe), markIfCaptured(Ident(sym)))
              } else {
                if (reifyDebug) println("Late local: " + sym)
                registerReifiableSymbol(sym)
                reifySymRef(sym)
              }
            }
        }
      }

      /**
       * reify the creation of a symbol
       */
      private def reifySymbolDef(sym: Symbol): Tree = {
        if (reifyDebug) println("reify sym def " + sym)

        ValDef(NoMods, localName(sym), TypeTree(),
          Apply(
            Select(reify(sym.owner), "newNestedSymbol"),
            List(reify(sym.name), reify(sym.pos), Literal(Constant(sym.flags)))
          )
        )
      }

      /**
       * Generate code to add type and annotation info to a reified symbol
       */
      private def fillInSymbol(sym: Symbol): Tree = {
        val rset = Apply(Select(reifySymRef(sym), nme.setTypeSignature), List(reifyType(sym.info)))
        if (sym.annotations.isEmpty) rset
        else Apply(Select(rset, nme.setAnnotations), List(reify(sym.annotations)))
      }

      /** Reify a scope */
      private def reifyScope(scope: Scope): Tree = {
        scope foreach registerReifiableSymbol
        mirrorCall(nme.newScopeWith, scope.toList map reifySymRef: _*)
      }

      /** Reify a list of symbols that need to be created */
      private def reifySymbols(syms: List[Symbol]): Tree = {
        syms foreach registerReifiableSymbol
        mkList(syms map reifySymRef)
      }

      /** Reify a type that defines some symbols */
      private def reifyTypeBinder(value: Product, bound: List[Symbol], underlying: Type): Tree =
        mirrorFactoryCall(value, reifySymbols(bound), reify(underlying))

      /** Reify a type */
      private def reifyType(tpe0: Type): Tree = {
        val tpe = tpe0.normalize

        if (tpe.isErroneous)
          CannotReifyErroneousType(tpe)
        if (definedInLiftedCode(tpe))
          CannotReifyTypeInvolvingBoundType(tpe)

        val tsym = tpe.typeSymbol
        if (tsym.isClass && tpe == tsym.typeConstructor && tsym.isStatic)
          Select(reifySymRef(tpe.typeSymbol), nme.asTypeConstructor)
        else tpe match {
          case t @ NoType =>
            reifyMirrorObject(t)
          case t @ NoPrefix =>
            reifyMirrorObject(t)
          case tpe @ ThisType(clazz) if clazz.isModuleClass && clazz.isStatic =>
            mirrorCall(nme.thisModuleType, reify(clazz.fullName))
          case t @ RefinedType(parents, decls) =>
            registerReifiableSymbol(tpe.typeSymbol)
            mirrorFactoryCall(t, reify(parents), reify(decls), reify(t.typeSymbol))
          case t @ ClassInfoType(parents, decls, clazz) =>
            registerReifiableSymbol(clazz)
            mirrorFactoryCall(t, reify(parents), reify(decls), reify(t.typeSymbol))
          case t @ ExistentialType(tparams, underlying) =>
            reifyTypeBinder(t, tparams, underlying)
          case t @ PolyType(tparams, underlying) =>
            reifyTypeBinder(t, tparams, underlying)
          case t @ MethodType(params, restpe) =>
            reifyTypeBinder(t, params, restpe)
          case t @ AnnotatedType(anns, underlying, selfsym) =>
            val saved1 = reifySymbols
            val saved2 = reifyTypes

            try {
              // one more quirk of reifying annotations
              //
              // when reifying AnnotatedTypes we need to reify all the types and symbols of inner ASTs
              // that's because a lot of logic expects post-typer trees to have non-null tpes
              //
              // Q: reified trees are pre-typer, so there's shouldn't be a problem.
              //    reflective typechecker will fill in missing symbols and types, right?
              // A: actually, no. annotation ASTs live inside AnnotatedTypes,
              //    and insides of the types is the place where typechecker doesn't look.
              reifySymbols = true
              reifyTypes = true
              if (reifyDebug) println("reify AnnotatedType: " + tpe)
              reifyProductUnsafe(tpe)
            } finally {
              reifySymbols = saved1
              reifyTypes = saved2
            }
          case _ =>
            reifyProductUnsafe(tpe)
        }
      }

      var reifySymbols = false
      var reifyTypes = false

      /** Preprocess a tree before reification */
      private def trimTree(tree: Tree): Tree = {
        def trimSyntheticCaseClassMembers(deff: Tree, stats: List[Tree]) = {
          var stats1 = stats filterNot (stat => stat.isDef && {
            if (stat.symbol.isCaseAccessorMethod && reifyDebug) println("discarding case accessor method: " + stat)
            stat.symbol.isCaseAccessorMethod
          })
          stats1 = stats1 filterNot (memberDef => memberDef.isDef && {
            val isSynthetic = memberDef.symbol.isSynthetic
            // @xeno.by: this doesn't work for local classes, e.g. for ones that are top-level to a quasiquote (see comments to companionClass)
            // that's why I replace the check with an assumption that all synthetic members are, in fact, generated of case classes
//            val isCaseMember = deff.symbol.isCaseClass || deff.symbol.companionClass.isCaseClass
            val isCaseMember = true
            if (isSynthetic && isCaseMember && reifyDebug) println("discarding case class synthetic def: " + memberDef)
            isSynthetic && isCaseMember
          })
          stats1 = stats1 map {
            case valdef @ ValDef(mods, name, tpt, rhs) if valdef.symbol.isCaseAccessor =>
              if (reifyDebug) println("resetting visibility of case accessor field: " + valdef)
              val Modifiers(flags, privateWithin, annotations) = mods
              val flags1 = flags & ~Flags.LOCAL & ~Flags.PRIVATE
              val mods1 = Modifiers(flags1, privateWithin, annotations)
              ValDef(mods1, name, tpt, rhs).copyAttrs(valdef)
            case stat =>
              stat
          }
          stats1
        }

        def trimSyntheticCaseClassCompanions(stats: List[Tree]) =
          stats diff (stats collect { case moddef: ModuleDef => moddef } filter (moddef => {
            val isSynthetic = moddef.symbol.isSynthetic
            // @xeno.by: this doesn't work for local classes, e.g. for ones that are top-level to a quasiquote (see comments to companionClass)
            // that's why I replace the check with an assumption that all synthetic modules are, in fact, companions of case classes
//            val isCaseCompanion = moddef.symbol.companionClass.isCaseClass
            val isCaseCompanion = true
            // @xeno.by: we also have to do this ugly hack for the very same reason described above
            // normally this sort of stuff is performed in reifyTree, which binds related symbols, however, local companions will be out of its reach
            if (reifyDebug) println("boundSym: "+  moddef.symbol)
            boundSyms += moddef.symbol
            if (isSynthetic && isCaseCompanion && reifyDebug) println("discarding synthetic case class companion: " + moddef)
            isSynthetic && isCaseCompanion
          }))

        tree match {
          case tree if tree.isErroneous =>
            tree
          case ta @ TypeApply(hk, ts) =>
            def isErased(tt: TypeTree) = tt.tpe != null && definedInLiftedCode(tt.tpe) && tt.original == null
            val discard = ts collect { case tt: TypeTree => tt } exists isErased
            if (reifyDebug && discard) println("discarding TypeApply: " + tree)
            if (discard) hk else ta
          case classDef @ ClassDef(mods, name, params, impl) =>
            val Template(parents, self, body) = impl
            val body1 = trimSyntheticCaseClassMembers(classDef, body)
            var impl1 = Template(parents, self, body1).copyAttrs(impl)
            ClassDef(mods, name, params, impl1).copyAttrs(classDef)
          case moduledef @ ModuleDef(mods, name, impl) =>
            val Template(parents, self, body) = impl
            val body1 = trimSyntheticCaseClassMembers(moduledef, body)
            var impl1 = Template(parents, self, body1).copyAttrs(impl)
            ModuleDef(mods, name, impl1).copyAttrs(moduledef)
          case template @ Template(parents, self, body) =>
            val body1 = trimSyntheticCaseClassCompanions(body)
            Template(parents, self, body1).copyAttrs(template)
          case block @ Block(stats, expr) =>
            val stats1 = trimSyntheticCaseClassCompanions(stats)
            Block(stats1, expr).copyAttrs(block)
          case valdef @ ValDef(mods, name, tpt, rhs) if valdef.symbol.isLazy =>
            if (reifyDebug) println("dropping $lzy in lazy val's name: " + tree)
            val name1 = if (name endsWith nme.LAZY_LOCAL) name dropRight nme.LAZY_LOCAL.length else name
            ValDef(mods, name1, tpt, rhs).copyAttrs(valdef)
          case unapply @ UnApply(fun, args) =>
            def extractExtractor(tree: Tree): Tree = {
              val Apply(fun, args) = tree
              args match {
                case List(Ident(special)) if special == nme.SELECTOR_DUMMY =>
                  val Select(extractor, flavor) = fun
                  assert(flavor == nme.unapply || flavor == nme.unapplySeq)
                  extractor
                case _ =>
                  extractExtractor(fun)
              }
            }

            if (reifyDebug) println("unapplying unapply: " + tree)
            val fun1 = extractExtractor(fun)
            Apply(fun1, args).copyAttrs(unapply)
          case _ =>
            tree
        }
      }

      /** Reify a tree */
      private def reifyTree(tree0: Tree): Tree = {
        val tree = trimTree(tree0)

        var rtree = tree match {
          case tree if tree.isErroneous =>
            CannotReifyErroneousTree(tree)
          case self.EmptyTree =>
            reifyMirrorObject(EmptyTree)
          case self.emptyValDef =>
            mirrorSelect(nme.emptyValDef)
          case This(_) if tree.symbol != NoSymbol && !(boundSyms contains tree.symbol) =>
            reifyFree(tree)
          case Ident(_) if tree.symbol != NoSymbol && !(boundSyms contains tree.symbol) =>
            if (tree.symbol.isVariable && tree.symbol.owner.isTerm) {
              if (reifyDebug) println("captured variable: " + tree.symbol)
              captureVariable(tree.symbol) // Note order dependency: captureVariable needs to come before reifyTree here.
              mirrorCall("Select", reifyFree(tree), reifyName(nme.elem))
            } else reifyFree(tree)
          case tt: TypeTree if (tt.tpe != null) =>
            reifyTypeTree(tt)
          case Literal(constant @ Constant(tpe: Type)) if boundSyms exists (tpe contains _) =>
            CannotReifyClassOfBoundType(tree, tpe)
          case Literal(constant @ Constant(sym: Symbol)) if boundSyms contains sym =>
            CannotReifyClassOfBoundEnum(tree, constant.tpe)
          case tree if tree.isDef =>
            if (reifyDebug) println("boundSym: %s of type %s".format(tree.symbol, (tree.productIterator.toList collect { case tt: TypeTree => tt } headOption).getOrElse(TypeTree(tree.tpe))))
            boundSyms += tree.symbol

            bindRelatedSymbol(tree.symbol.sourceModule, "sourceModule")
            bindRelatedSymbol(tree.symbol.moduleClass, "moduleClass")
            bindRelatedSymbol(tree.symbol.companionClass, "companionClass")
            bindRelatedSymbol(tree.symbol.companionModule, "companionModule")
            Some(tree.symbol) collect { case termSymbol: TermSymbol => bindRelatedSymbol(termSymbol.referenced, "referenced") }
            def bindRelatedSymbol(related: Symbol, name: String): Unit =
              if (related != null && related != NoSymbol) {
                if (reifyDebug) println("boundSym (" + name + "): " + related)
                boundSyms += related
              }

            val prefix = tree.productPrefix
            val elements = (tree.productIterator map {
              // annotations exist in two flavors:
              // 1) pre-typer ones that populate: a) Modifiers, b) Annotated nodes (irrelevant in this context)
              // 2) post-typer ones that dwell inside: a) sym.annotations, b) AnnotatedTypes (irrelevant in this context)
              //
              // here we process Modifiers that are involved in deftrees
              // AnnotatedTypes get reified elsewhere (currently, in ``reifyTypeTree'')
              case Modifiers(flags, privateWithin, annotations) =>
                assert(annotations.isEmpty) // should've been eliminated by the typer
                val postTyper = tree.symbol.annotations filter (_.original != EmptyTree)
                if (reifyDebug && !postTyper.isEmpty) println("reify symbol annotations for %s: %s".format(tree.symbol, tree.symbol.annotations))
                val preTyper = postTyper map toPreTyperAnnotation
                Modifiers(flags, privateWithin, preTyper)
              case x =>
                x
            }).toList
            reifyProduct(prefix, elements)
          case _ =>
            reifyProduct(tree)
        }

        // usually we don't reify symbols/types, because they can be re-inferred during subsequent reflective compilation
        // however, reification of AnnotatedTypes is special. see ``reifyType'' to find out why.
        if (reifySymbols && tree.hasSymbol) {
          if (reifyDebug) println("reifying symbol %s for tree %s".format(tree.symbol, tree))
          rtree = Apply(Select(rtree, nme.setSymbol), List(reifySymRef(tree.symbol)))
        }
        if (reifyTypes && tree.tpe != null) {
          if (reifyDebug) println("reifying type %s for tree %s".format(tree.tpe, tree))
          rtree = Apply(Select(rtree, nme.setType), List(reifyType(tree.tpe)))
        }

        rtree
      }

      /** Reify pre-typer representation of a type.
       *
       *  NB: This is the trickiest part of reification!
       *
       *  In most cases, we're perfectly fine to reify a Type itself (see ``reifyType'').
       *  However if the type involves a symbol declared inside the quasiquote (i.e. registered in ``boundSyms''),
       *  then we cannot reify it, or otherwise subsequent reflective compilation will fail.
       *
       *  Why will it fail? Because reified deftrees (e.g. ClassDef(...)) will generate fresh symbols during that compilation,
       *  so naively reified symbols will become out of sync, which brings really funny compilation errors and/or crashes, e.g.:
       *  https://issues.scala-lang.org/browse/SI-5230
       *
       *  To deal with this unpleasant fact, we need to fall back from types to equivalent trees (after all, parser trees don't contain any types, just trees, so it should be possible).
       *  Luckily, these original trees get preserved for us in the ``original'' field when Trees get transformed into TypeTrees.
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
       *  This is laboriously worked around in the code below. I hope this will be the only workaround in this department.
       */
      private def reifyTypeTree(tt: TypeTree): Tree = {
        if (definedInLiftedCode(tt.tpe)) {
          if (reifyDebug) println("reifyTypeTree, defined in lifted code: " + tt.tpe)
          if (tt.original != null) {
            val annotations = tt.tpe filter { _.isInstanceOf[AnnotatedType] } collect { case atp: AnnotatedType => atp.annotations } flatten
            val annmap = annotations map { ann => (ann.original, ann) } toMap

            // annotations exist in two flavors:
            // 1) pre-typer ones that populate: a) Modifiers (irrelevant in this context), b) Annotated nodes
            // 2) post-typer ones that dwell inside: a) sym.annotations (irrelevant in this context), b) AnnotatedTypes
            //
            // here we process AnnotatedTypes, since only they can be involved in TypeTrees
            // Modifiers get reified elsewhere (currently, in the "isDef" case of ``reifyTree'')
            //
            // the problem with annotations is that their originals don't preserve any symbols at all
            // read the comment to this method to find out why it's bad
            // that's why we transplant typechecked, i.e. symful, annotations onto original trees
            class AnnotationFixup extends self.Transformer {
              override def transform(tree: Tree) = tree match {
                case Annotated(ann0, args) =>
                  assert(annmap contains ann0)
                  val ann1 = annmap(ann0)
                  val ann = toPreTyperAnnotation(ann1)
                  Annotated(ann, transform(args))
                case _ =>
                  tree
              }
            }

            if (reifyDebug) println("verdict: essential, reify as original")
            val patchedOriginal = new AnnotationFixup().transform(tt.original)
            reifyTree(patchedOriginal)
          } else {
            // type is deemed to be non-essential
            // erase it and hope that subsequent reflective compilation will be able to recreate it again
            if (reifyDebug) println("verdict: non-essential, discard")
            mirrorCall("TypeTree")
          }
        } else {
          var rtt = mirrorCall(nme.TypeTree, reifyType(tt.tpe))
          // @xeno.by: temporarily disabling reification of originals
          // subsequent reflective compilation will try to typecheck them
          // and this means that the reifier has to do additional efforts to ensure that this will succeed
          // additional efforts + no clear benefit = will be implemented later
//          if (tt.original != null) {
//            val setOriginal = Select(rtt, newTermName("setOriginal"))
//            val reifiedOriginal = reify(tt.original)
//            rtt = Apply(setOriginal, List(reifiedOriginal))
//          }
          rtt
        }
      }

      /** Reify post-typer representation of an annotation */
      private def reifyAnnotation(ann: AnnotationInfo): Tree =
        // @xeno.by: if you reify originals, you get SO when trying to reify AnnotatedTypes, so screw it - after all, it's not that important
        mirrorFactoryCall("AnnotationInfo", reifyType(ann.atp), reifyList(ann.args), reify(ann.assocs))

      /** Reify pre-typer representation of an annotation.
       *  The trick here is to retain the symbols that have been populated during typechecking of the annotation.
       *  If we do not do that, subsequent reflective compilation will fail.
       */
      private def toPreTyperAnnotation(ann: AnnotationInfo): Tree = {
        if (definedInLiftedCode(ann.atp)) {
          // todo. deconstruct reifiable tree from ann.original and ann.args+ann.assocs
          //
          // keep in mind that we can't simply use ann.original, because its args are symless
          // which means that any imported symbol (e.g. List) will crash subsequent reflective compilation
          // hint: if I had enough time, I'd try to extract reifiable annotation type from ann.original
          // and to apply its constructor to ann.args (that are symful, i.e. suitable for reification)
          //
          // also, if we pursue the route of reifying annotations defined in lifted code
          // we should think about how to provide types for all nodes of the return value
          // this will be necessary for reifying AnnotatedTypes, since ASTs inside ATs must all have non-null tpes
          // an alternative would be downgrading ATs to Annotated nodes, but this needs careful thinking
          // for now I just leave this as an implementation restriction
          CannotReifyAnnotationInvolvingBoundType(ann)
        } else {
          val args = if (ann.assocs.isEmpty) {
            ann.args
          } else {
            def toScalaAnnotation(jann: ClassfileAnnotArg): Tree = jann match {
              case LiteralAnnotArg(const) =>
                Literal(const)
              case ArrayAnnotArg(arr) =>
                Apply(Ident(definitions.ArrayModule), arr.toList map toScalaAnnotation)
              case NestedAnnotArg(ann) =>
                toPreTyperAnnotation(ann)
            }

            ann.assocs map { case (nme, arg) => AssignOrNamedArg(Ident(nme), toScalaAnnotation(arg)) }
          }

          New(TypeTree(ann.atp), List(args))
        }
      }

      /**
       * Reify a free reference. The result will be either a mirror reference
       *  to a global value, or else a mirror Literal.
       */
      private def reifyFree(tree: Tree): Tree = tree match {
        case This(_) if tree.symbol.isClass && !tree.symbol.isModuleClass =>
          val sym = tree.symbol
          if (reifyDebug) println("This for %s, reified as freeVar".format(sym))
          if (reifyDebug) println("Free: " + sym)
          val freeVar = mirrorCall("newFreeVar", reify(sym.name.toString), reify(sym.tpe), This(sym))
          mirrorCall(nme.Ident, freeVar)
        case This(_) =>
          if (reifyDebug) println("This for %s, reified as This".format(tree.symbol))
          mirrorCall(nme.This, reifySymRef(tree.symbol))
        case _ =>
          mirrorCall(nme.Ident, reifySymRef(tree.symbol))
      }

      // todo: consider whether we should also reify positions
      private def reifyPosition(pos: Position): Tree =
        reifyMirrorObject(NoPosition)

      // !!! we must eliminate these casts.
      private def reifyProductUnsafe(x: Any): Tree =
        if (x.isInstanceOf[Product]) reifyProduct(x.asInstanceOf[Product])
        else throw new Exception("%s of type %s cannot be cast to Product".format(x, x.getClass))
      private def reifyProduct(x: Product): Tree =
        reifyProduct(x.productPrefix, x.productIterator.toList)
      private def reifyProduct(prefix: String, elements: List[Any]): Tree = {
        // @xeno.by: reflection would be more robust, but, hey, this is a hot path
        if (prefix.startsWith("Tuple")) reifyAggregate(prefix, elements: _*)
        else mirrorCall(prefix, (elements map reify): _*)
      }

      /**
       * Reify a case object defined in Mirror
       */
      private def reifyMirrorObject(name: String): Tree = mirrorSelect(name)
      private def reifyMirrorObject(x: Product): Tree   = reifyMirrorObject(x.productPrefix)

      private def isReifiableConstant(value: Any) = value match {
        case null      => true  // seems pretty reifable to me?
        case _: String => true
        case _         => isAnyVal(value)
      }

      /** Reify an arbitary value */
      private def reify(value: Any): Tree = value match {
        case tree: Tree         => reifyTree(tree)
        case sym: Symbol        => reifySymRef(sym)
        case tpe: Type          => reifyType(tpe)
        case xs: List[_]        => reifyList(xs)
        case xs: Array[_]       => reifyArray(xs)
        case scope: Scope       => reifyScope(scope)
        case x: Name            => reifyName(x)
        case x: Position        => reifyPosition(x)
        case x: Modifiers       => reifyModifiers(x)
        case x: AnnotationInfo  => reifyAnnotation(x)
        case _ =>
          if (isReifiableConstant(value)) Literal(Constant(value))
          else reifyProductUnsafe(value)
      }

      /**
       * An (unreified) path that refers to definition with given fully qualified name
       *  @param mkName   Creator for last portion of name (either TermName or TypeName)
       */
      private def path(fullname: String, mkName: String => Name): Tree = {
        val parts = fullname split "\\."
        val prefixParts = parts.init
        val lastName = mkName(parts.last)
        if (prefixParts.isEmpty) Ident(lastName)
        else {
          val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail)(Select(_, _))
          Select(prefixTree, lastName)
        }
      }

      /** An (unreified) path that refers to term definition with given fully qualified name */
      private def termPath(fullname: String): Tree = path(fullname, newTermName)

      /** An (unreified) path that refers to type definition with given fully qualified name */
      private def typePath(fullname: String): Tree = path(fullname, newTypeName)

      private def mirrorAlias =
        ValDef(NoMods, nme.MIRROR_SHORT, SingletonTypeTree(termPath(fullnme.MirrorPackage)), termPath(fullnme.MirrorPackage))

      /**
       * Generate code that generates a symbol table of all symbols registered in `reifiableSyms`
       */
      private def reifySymbolTableSetup: List[Tree] = {
        val symDefs, fillIns = new mutable.ArrayBuffer[Tree]
        var i = 0
        while (i < reifiableSyms.length) {
          // fillInSymbol might create new reifiableSyms, that's why this is done iteratively
          symDefs += reifySymbolDef(reifiableSyms(i))
          fillIns += fillInSymbol(reifiableSyms(i))
          i += 1
        }

        symDefs.toList ++ fillIns.toList
      }
    } // end of Reifier

    object Reifier {
      def CannotReifyPreTyperTree(tree: Tree) = {
        val msg = "pre-typer trees are not supported, consider typechecking the tree before passing it to the reifier"
        throw new ReifierError(tree.pos, msg)
      }

      def CannotReifyErroneousTree(tree: Tree) = {
        val msg = "erroneous trees are not supported, make sure that your tree typechecks successfully before passing it to the reifier"
        throw new ReifierError(tree.pos, msg)
      }

      def CannotReifyErroneousType(tpe: Type) = {
        val msg = "erroneous types are not supported, make sure that your tree typechecks successfully before passing it to the reifier"
        throw new ReifierError(NoPosition, msg)
      }

      def CannotReifyClassOfBoundType(tree: Tree, tpe: Type) = {
        val msg = "implementation restriction: cannot reify classOf[%s] which refers to a type declared inside the block being reified".format(tpe)
        throw new ReifierError(tree.pos, msg)
      }

      def CannotReifyClassOfBoundEnum(tree: Tree, tpe: Type) = {
        val msg = "implementation restriction: cannot reify classOf[%s] which refers to an enum declared inside the block being reified".format(tpe)
        throw new ReifierError(tree.pos, msg)
      }

      def CannotReifyTypeInvolvingBoundType(tpe: Type) = {
        val msg = "implementation restriction: cannot reify type %s which involves a symbol declared inside the block being reified".format(tpe)
        throw new ReifierError(NoPosition, msg)
      }

      def CannotReifyAnnotationInvolvingBoundType(ann: AnnotationInfo) = {
        val msg = "implementation restriction: cannot reify annotation @%s which involves a symbol declared inside the block being reified".format(ann)
        throw new ReifierError(ann.original.pos, msg)
      }
    } // end of Reifier

    // begin reify
    import Reifier._
    if (tree.tpe != null) {
      val saved = printTypings
      try {
        val reifyDebug = settings.Yreifydebug.value
        val debugTrace = util.trace when reifyDebug
        debugTrace("transforming = ")(if (settings.Xshowtrees.value) "\n" + nodePrinters.nodeToString(tree).trim else tree.toString)
        debugTrace("transformed = ") {
          val reifier = new Reifier()
          val untyped = reifier.reifyTopLevel(tree)

          val reifyCopypaste = settings.Yreifycopypaste.value
          if (reifyCopypaste) {
            if (reifyDebug) println("=======================")
            println(reifiedNodeToString(untyped))
            if (reifyDebug) println("=======================")
          }

          untyped
        }
      } finally {
        printTypings = saved
      }
    } else {
      CannotReifyPreTyperTree(tree)
    }
  }

  /** A throwable signalling a reification error */
  class ReifierError(var pos: Position, val msg: String) extends Throwable(msg) {
    def this(msg: String) = this(NoPosition, msg)
  }
}
