/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import scala.collection.mutable

/**
 * Extracts simple names used in given compilation unit.
 *
 * Extracts simple (unqualified) names mentioned in given in non-definition position by collecting
 * all symbols associated with non-definition trees and extracting names from all collected symbols.
 * Also extract the names of the types of non-definition trees (see source-dependencies/types-in-used-names-*
 * and source-dependencies/as-seen-from-* for examples where this is required).
 *
 * If given symbol is mentioned both in definition and in non-definition position (e.g. in member
 * selection) then that symbol is collected. It means that names of symbols defined and used in the
 * same compilation unit are extracted. We've considered not extracting names of those symbols
 * as an optimization strategy. It turned out that this is not correct.  Check
 * https://github.com/gkossakowski/sbt/issues/3 for an example of scenario where it matters.
 *
 * All extracted names are returned in _decoded_ form. This way we stay consistent with the rest
 * of incremental compiler which works with names in decoded form.
 *
 * Names mentioned in Import nodes are handled properly but require some special logic for two
 * reasons:
 *
 *   1. The `termSymbol` of Import nodes point to the symbol of the prefix it imports from
 *      (not the actual members that we import, that are represented as names).
 *   2. ImportSelector is not subtype of Tree therefore is not processed by `Tree.foreach`.
 *
 * Another type of tree nodes that requires special handling is TypeTree. TypeTree nodes
 * has a little bit odd representation:
 *
 *   1. TypeTree.hasSymbol always returns false even when TypeTree.symbol
 *      returns a symbol
 *   2. The original tree from which given TypeTree was derived is stored
 *      in TypeTree.original but Tree.forech doesn't walk into original
 *      tree so we missed it
 *
 * The tree walking algorithm walks into TypeTree.original explicitly.
 *
 */
class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType) extends ClassName with GlobalHelpers {
  import global._

  def extract(unit: CompilationUnit): Iterable[(String, Iterable[String])] = {
    val tree = unit.body
    val traverser = new ExtractUsedNamesTraverser
    traverser.traverse(tree)
    val namesUsedAtTopLevel = traverser.namesUsedAtTopLevel

    // Decode scala name (e.g. operator).
    // This is copied from Names$Name to call it once on given name (at this time we don't have names anymore)
    def decodeName(name: String): String = {
      val decoded = if (name.contains("$")) reflect.NameTransformer.decode(name) else name
      decoded.trim
    }

    if (namesUsedAtTopLevel.nonEmpty) {
      val classOrModuleDef = firstClassOrModuleDef(tree)
      classOrModuleDef match {
        case Some(classOrModuleDef) =>
          val sym = classOrModuleDef.symbol
          val firstClassSymbol = if (sym.isModule) sym.moduleClass else sym
          val firstClassName = className(firstClassSymbol)
          traverser.usedNamesFromClass(firstClassName) ++= namesUsedAtTopLevel.map(decodeName)
        case None =>
          reporter.warning(unit.position(0), Feedback.OrphanNames)
      }
    }

    traverser.usedNamesFromClasses.map {
      case (name, names) =>
        name -> names.map(decodeName)
    }
  }

  private def firstClassOrModuleDef(tree: Tree): Option[Tree] = {
    tree foreach {
      case t @ ((_: ClassDef) | (_: ModuleDef)) => return Some(t)
      case _                                    => ()
    }
    None
  }

  private class ExtractUsedNamesTraverser extends Traverser {
    val usedNamesFromClasses = mutable.Map.empty[String, mutable.Set[String]]
    val namesUsedAtTopLevel = mutable.Set.empty[String]

    override def traverse(tree: Tree): Unit = {
      handleClassicTreeNode(tree)
      processMacroExpansion(tree)(handleMacroExpansion)
      super.traverse(tree)
    }

    val addSymbol: Symbol => Unit = {
      symbol =>
        val enclosingNonLocalClass = resolveEnclosingNonLocalClass
        if (enclosingNonLocalClass.symbolsCache.add(symbol) && eligibleAsUsedName(symbol))
          enclosingNonLocalClass.addName(symbol.name)
    }

    /** Returns mutable set with all names from given class used in current context */
    def usedNamesFromClass(className: String): collection.mutable.Set[String] = {
      usedNamesFromClasses.get(className) match {
        case None =>
          val emptySet = scala.collection.mutable.Set.empty[String]
          usedNamesFromClasses.put(className, emptySet)
          emptySet
        case Some(setForClass) => setForClass
      }
    }

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    private val inspectedOriginalTrees = collection.mutable.Set.empty[Tree]
    private val inspectedTypeTrees = collection.mutable.Set.empty[Tree]

    private val handleMacroExpansion: Tree => Unit =
      original => if (inspectedOriginalTrees.add(original)) traverse(original)

    private def handleClassicTreeNode(tree: Tree): Unit = tree match {
      case _: DefTree | _: Template => ()
      case Import(_, selectors: List[ImportSelector]) =>
        val enclosingNonLocalClass = resolveEnclosingNonLocalClass()
        def usedNameInImportSelector(name: Name): Unit =
          if ((name != null) && (name != nme.WILDCARD)) enclosingNonLocalClass.addName(name)
        selectors foreach { selector =>
          usedNameInImportSelector(selector.name)
          usedNameInImportSelector(selector.rename)
        }
      // TODO: figure out whether we should process the original tree or walk the type
      // the argument for processing the original tree: we process what user wrote
      // the argument for processing the type: we catch all transformations that typer applies
      // to types but that might be a bad thing because it might expand aliases eagerly which
      // not what we need
      case t: TypeTree if t.original != null =>
        if (inspectedTypeTrees.add(t.original)) {
          t.original.foreach(traverse)
        }
      case t if t.hasSymbolField =>
        addSymbol(t.symbol)
        if (t.tpe != null)
          foreachNotPackageSymbolInType(t.tpe)(addSymbol)
      case _ =>
    }

    private case class EnclosingNonLocalClass(currentOwner: Symbol) {
      val symbolsCache = mutable.Set.empty[Symbol]

      private val usedNamesSet: collection.mutable.Set[String] = {
        val fromClass = enclOrModuleClass(currentOwner)
        if (fromClass == NoSymbol || fromClass.hasPackageFlag)
          namesUsedAtTopLevel
        else {
          val fromNonLocalClass = localToNonLocalClass.resolveNonLocal(fromClass)
          usedNamesFromClass(ExtractUsedNames.this.className(fromNonLocalClass))
        }
      }

      def addName(name: Name): Unit = {
        usedNamesSet.add(name.toString)
        ()
      }
    }

    private var _lastEnclosingNonLocalClass: EnclosingNonLocalClass = null

    /**
     * Resolves a class to which we attribute a used name by getting the enclosing class
     * for `currentOwner` and then looking up the most inner enclosing class that is non local.
     * The second returned value indicates if the enclosing class for `currentOwner`
     * is a local class.
     */
    private def resolveEnclosingNonLocalClass(): EnclosingNonLocalClass = {
      def newOne(): EnclosingNonLocalClass = {
        _lastEnclosingNonLocalClass = EnclosingNonLocalClass(currentOwner)
        _lastEnclosingNonLocalClass
      }

      _lastEnclosingNonLocalClass match {
        case null =>
          newOne()
        case cached @ EnclosingNonLocalClass(owner) if owner == currentOwner =>
          cached
        case _ =>
          newOne()
      }
    }

    private def enclOrModuleClass(s: Symbol): Symbol =
      if (s.isModule) s.moduleClass else s.enclClass
  }

  private def eligibleAsUsedName(symbol: Symbol): Boolean = {
    // Synthetic names are no longer included. See https://github.com/sbt/sbt/issues/2537
    !ignoredSymbol(symbol) && !isEmptyName(symbol.name)
  }
}
