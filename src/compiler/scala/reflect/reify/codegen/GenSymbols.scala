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

package scala.reflect.reify
package codegen

import scala.reflect.internal.Flags._

trait GenSymbols {
  self: Reifier =>

  import global._

  /** Symbol table of the reifee.
   *
   *  Keeps track of auxiliary symbols that are necessary for this reification session.
   *  These include:
   *    1) Free vars (terms, types and existentials),
   *    2) Non-locatable symbols (sometimes, e.g. for RefinedTypes, we need to reify these; to do that we create their copies in the reificode)
   *    3) Non-locatable symbols that are referred by #1, #2 and #3
   *
   *  Exposes three main methods:
   *    1) `syms` that lists symbols belonging to the table,
   *    2) `symXXX` family of methods that provide information about the symbols in the table,
   *    3) `encode` that renders the table into a list of trees (recursively populating #3 and setting up initialization code for #1, #2 and #3)
   */
  def symtab: SymbolTable = state.symtab

  /** Reify a reference to a symbol */
  def reifySymRef(sym: Symbol): Tree = {
    assert(sym != null, "sym is null")
    if (sym == NoSymbol)
      mirrorSelect(nme.NoSymbol)
    else if (sym.isRootPackage)
      mirrorMirrorSelect(nme.RootPackage)
    else if (sym.isRoot)
      mirrorMirrorSelect(nme.RootClass)
    else if (sym.isEmptyPackage)
      mirrorMirrorSelect(nme.EmptyPackage)
    else if (sym.isEmptyPackageClass)
      mirrorMirrorSelect(nme.EmptyPackageClass)
    else if (sym.isModuleClass)
      if (sym.sourceModule.isLocatable) Select(Select(reify(sym.sourceModule), nme.asModule), nme.moduleClass)
      else reifySymDef(sym)
    else if (sym.hasPackageFlag)
      mirrorMirrorCall(nme.staticPackage, reify(sym.fullName))
    else if (sym.isLocatable) {
      /*  This is a fancy conundrum that stems from the fact that Scala allows
       *  packageless packages and packageless objects with the same names in the same program.
       *
       *  For more details read the docs to staticModule and staticPackage.
       *  Here I'll just provide the examples of how reify works for different kinds of symbols.
       *
       *    // 1) packageless
       *    // packageless classes are non-ambiguous, but modules vs packages might be
       *    // that's why we have separate methods to reify those
       *    // note that staticModule will never resolve to a package if an object is missing and an homonymous package is present and vice versa
       *    // otherwise reification would be unsound
       *    class C => staticClass("C")
       *    object B => staticModule("B")
       *    package B => staticPackage("B")
       *
       *    // 2) classes and modules enclosed in a package
       *    // staticXXX methods always look into parent packages and ignores parent modules, so for fully qualified names they are non-ambiguous
       *    // namely even if there's an object B { class C } next to package B { class C }, then staticClass("B.C") will resolve to a packageful class
       *    // this closely mirrors Scala's behavior, read up the docs to staticModule/staticPackage for more information
       *    package B { class C } => staticClass("B.C")
       *    package B { object B } => staticModule("B.B")
       *    package B { package B } => staticPackage("B.B")
       *
       *    // 3) classes and modules enclosed in a packageless module
       *    // staticClass/staticModule won't look into EmptyPackageClass, so we reify such symbols in a roundabout way
       *    object B { class C } => selectType(staticModule("B"), "C")
       *    object B { object B } => selectType(staticModule("B"), "B")
       *    object B { package B } => impossible
       */
      val hasPackagelessParent = sym.ownerChain.tail.tail exists (_.isEmptyPackageClass)
      if (sym.isStatic && (sym.isClass || sym.isModule) && !hasPackagelessParent) {
        // scala/bug#6238: if applicable, emit references to StandardDefinitions instead of staticClass/staticModule calls
        val resolver = if (sym.isType) nme.staticClass else nme.staticModule
        mirrorMirrorCall(resolver, reify(sym.fullName))
      } else {
        if (reifyDebug) println("Locatable: %s (%s) owned by %s (%s) at %s".format(sym, sym.accurateKindString, sym.owner, sym.owner.accurateKindString, sym.owner.fullNameString))
        val rowner = reify(sym.owner)
        val rname = reify(sym.name.toString)
        if (sym.isType)
          mirrorBuildCall(nme.selectType, rowner, rname)
        else if (sym.isMethod && sym.owner.isClass && sym.owner.info.decl(sym.name).isOverloaded) {
          val index = sym.owner.info.decl(sym.name).alternatives indexOf sym
          assert(index >= 0, sym)
          mirrorBuildCall(nme.selectOverloadedMethod, rowner, rname, reify(index))
        } else
          mirrorBuildCall(nme.selectTerm, rowner, rname)
      }
    } else {
      // todo. make sure that free methods work correctly
      if (sym.isExistential) reifySymDef(sym)
      else if (sym.isTerm) reifyFreeTerm(Ident(sym))
      else reifyFreeType(Ident(sym)) // TODO: reify refinement classes
    }
  }

  def reifyFreeTerm(binding: Tree): Tree =
    reifyIntoSymtab(binding.symbol) { sym =>
      if (reifyDebug) println("Free term" + (if (sym.isCapturedVariable) " (captured)" else "") + ": " + sym + "(" + sym.accurateKindString + ")")
      val name = newTermName("" + nme.REIFY_FREE_PREFIX + sym.name + (if (sym.isType) nme.REIFY_FREE_THIS_SUFFIX else ""))
      // We need to note whether the free value being reified is stable or not to guide subsequent reflective compilation.
      // Here's why reflection compilation needs our help.
      //
      // When dealing with a tree, which contain free values, toolboxes extract those and wrap the entire tree in a Function
      // having parameters defined for every free values in the tree. For example, evaluating
      //
      //   Ident(setTypeSignature(newFreeTerm("x", 2), <Int>))
      //
      // Will generate something like
      //
      //   object wrapper {
      //     def wrapper(x: () => Int) = {
      //       x()
      //     }
      //   }
      //
      // Note that free values get transformed into, effectively, by-name parameters. This is done to make sure
      // that evaluation order is kept intact. And indeed, we cannot just evaluate all free values at once in order
      // to obtain arguments for wrapper.wrapper, because if some of the free values end up being unused during evaluation,
      // we might end up doing unnecessary calculations.
      //
      // So far, so good - we didn't need any flags at all. However, if the code being reified contains path-dependent types,
      // we're in trouble, because valid code like `free.T` ends up being transformed into `free.apply().T`, which won't compile.
      //
      // To overcome this glitch, we note whether a given free term is stable or not (because vars can also end up being free terms).
      // Then, if a free term is stable, we tell the compiler to treat `free.apply()` specially and assume that it's stable.
      if (!sym.isMutable) sym setFlag STABLE
      if (sym.isCapturedVariable) {
        assert(binding.isInstanceOf[Ident], showRaw(binding))
        val capturedBinding = referenceCapturedVariable(sym)
        Reification(name, capturedBinding, mirrorBuildCall(nme.newFreeTerm, reify(sym.name.toString), capturedBinding, mirrorBuildCall(nme.FlagsRepr, reify(sym.flags)), reify(origin(sym))))
      } else {
        Reification(name, binding, mirrorBuildCall(nme.newFreeTerm, reify(sym.name.toString), binding, mirrorBuildCall(nme.FlagsRepr, reify(sym.flags)), reify(origin(sym))))
      }
    }

  def reifyFreeType(binding: Tree): Tree =
    reifyIntoSymtab(binding.symbol) { sym =>
      if (reifyDebug) println("Free type: %s (%s)".format(sym, sym.accurateKindString))
      state.reificationIsConcrete = false
      val name: TermName = nme.REIFY_FREE_PREFIX append sym.name
      Reification(name, binding, mirrorBuildCall(nme.newFreeType, reify(sym.name.toString), mirrorBuildCall(nme.FlagsRepr, reify(sym.flags)), reify(origin(sym))))
    }

  def reifySymDef(sym: Symbol): Tree =
    reifyIntoSymtab(sym) { sym =>
      if (reifyDebug) println("Sym def: %s (%s)".format(sym, sym.accurateKindString))
      val name: TermName = nme.REIFY_SYMDEF_PREFIX append sym.name
      def reifiedOwner = if (sym.owner.isLocatable) reify(sym.owner) else reifySymDef(sym.owner)
      Reification(name, Ident(sym), mirrorBuildCall(nme.newNestedSymbol, reifiedOwner, reify(sym.name), reify(sym.pos), mirrorBuildCall(nme.FlagsRepr, reify(sym.flags)), reify(sym.isClass)))
    }

  case class Reification(name: Name, binding: Tree, tree: Tree)

  private def reifyIntoSymtab(sym: Symbol)(reificode: Symbol => Reification): Tree = {
    def fromSymtab = symtab symRef sym
    if (fromSymtab == EmptyTree) {
      // reification is lazy, so that we can carefully choose where to evaluate it
      // and we choose this place to be exactly here:
      //
      // reasons:
      // 1) reification happens at maximum once per symbol to prevent repeated reifications
      // 2) reification happens before putting the symbol itself into the symbol table to ensure correct initialization order:
      //    for example, if reification of symbol A refers to reification of symbol B
      //    (this might happen when we're doing `reifySymDef`, which expands into `newNestedSymbol`, which needs `sym.owner`)
      //    then we have to put reification-B into the symbol table before reification-A
      //    so that subsequent code generation that traverses the symbol table in the first-added first-codegenned order
      //    produces valid Scala code (with vals in a block depending only on lexically preceding vals)
      val reification = reificode(sym)
      import reification.{name, binding}
      val tree = reification.tree updateAttachment ReifyBindingAttachment(binding)
      state.symtab = state.symtab.add(sym, name.toTermName, tree)
    }
    fromSymtab
  }
}
