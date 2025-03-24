/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import java.util.{ HashMap => JavaMap }
import java.util.{ HashSet => JavaSet }
import java.util.EnumSet

import xsbti.UseScope

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
class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType)
    extends Compat
    with ClassName
    with GlobalHelpers {

  import global._
  import JavaUtils._

  private final class NamesUsedInClass {
    // Default names and other scopes are separated for performance reasons
    val defaultNames: JavaSet[Name] = new JavaSet[global.Name]()
    val scopedNames: JavaMap[Name, EnumSet[UseScope]] = new JavaMap[Name, EnumSet[UseScope]]()

    // We have to leave with commas on ends
    override def toString(): String = {
      val builder = new StringBuilder(": ")
      defaultNames.foreach { name =>
        builder.append(name.decoded.trim)
        val otherScopes = scopedNames.get(name)
        if (otherScopes != null) {
          builder.append(" in [")
          otherScopes.foreach(scope => builder.append(scope.name()).append(", "))
          builder.append("]")
        }
        builder.append(", ")
      }
      builder.toString()
    }
  }

  private def DefaultScopes = EnumSet.of(UseScope.Default)
  private def PatmatScopes = EnumSet.of(UseScope.PatMatTarget)

  def extractAndReport(unit: CompilationUnit): Unit = {
    val tree = unit.body
    val traverser = new ExtractUsedNamesTraverser
    traverser.traverse(tree)

    val namesUsedAtTopLevel = traverser.namesUsedAtTopLevel
    val defaultNamesTopLevel = namesUsedAtTopLevel.defaultNames
    val scopedNamesTopLevel = namesUsedAtTopLevel.scopedNames

    // Handle names used at top level that cannot be related to an owner
    if (!defaultNamesTopLevel.isEmpty || !scopedNamesTopLevel.isEmpty) {
      val responsible = firstClassOrModuleDef(tree)
      responsible match {
        case Some(classOrModuleDef) =>
          val sym = classOrModuleDef.symbol
          val firstClassSymbol = enclOrModuleClass(sym)
          val firstClassName = className(firstClassSymbol)
          val namesInFirstClass = traverser.usedNamesFromClass(firstClassName)
          val scopedNamesInFirstClass = namesInFirstClass.scopedNames

          namesInFirstClass.defaultNames.addAll(defaultNamesTopLevel)
          scopedNamesTopLevel.foreach { (topLevelName, topLevelScopes) =>
            val existingScopes = scopedNamesInFirstClass.get(topLevelName)
            if (existingScopes == null)
              scopedNamesInFirstClass.put(topLevelName, topLevelScopes)
            else existingScopes.addAll(topLevelScopes)
            ()
          }

        case None =>
          reporter.echo(unit.position(0), Feedback.OrphanNames)
      }
    }

    debuglog {
      val msg = s"The ${unit.source} contains the following used names:\n"
      val builder = new StringBuilder(msg)
      traverser.usedNamesFromClasses.foreach { (name, usedNames) =>
        builder
          .append(name.toString.trim)
          .append(": ")
          .append(usedNames.toString())
          .append("\n")
        ()
      }
      builder.toString()
    }

    // Handle names circumscribed to classes
    traverser.usedNamesFromClasses.foreach { (rawClassName, usedNames) =>
      val className = rawClassName.toString.trim.intern()
      usedNames.defaultNames.foreach { rawUsedName =>
        val useName = rawUsedName.decoded.trim.intern()
        val existingScopes = usedNames.scopedNames.get(rawUsedName)
        val useScopes = {
          if (existingScopes == null) DefaultScopes
          else {
            existingScopes.add(UseScope.Default)
            existingScopes
          }
        }
        callback.usedName(className, useName, useScopes)
      }
    }
  }

  private def firstClassOrModuleDef(tree: Tree): Option[Tree] = {
    tree find {
      case ((_: ClassDef) | (_: ModuleDef)) => true
      case _                                => false
    }
  }

  private class ExtractUsedNamesTraverser extends Traverser {

    val usedNamesFromClasses = new JavaMap[Name, NamesUsedInClass]()
    val namesUsedAtTopLevel = new NamesUsedInClass

    override def traverse(tree: Tree): Unit = {
      handleClassicTreeNode(tree)
      processMacroExpansion(tree)(handleMacroExpansion)
      super.traverse(tree)
    }

    val addSymbol: (JavaSet[Name], Symbol) => Unit = { (names: JavaSet[Name], symbol: Symbol) =>
      // Synthetic names are no longer included. See https://github.com/sbt/sbt/issues/2537
      if (!ignoredSymbol(symbol) && !isEmptyName(symbol.name)) {
        names.add(mangledName(symbol))
        ()
      }
    }

    /** Returns mutable set with all names from given class used in current context */
    def usedNamesFromClass(className: Name): NamesUsedInClass = {
      val names = usedNamesFromClasses.get(className)
      if (names == null) {
        val newOne = new NamesUsedInClass
        usedNamesFromClasses.put(className, newOne)
        newOne
      } else names
    }

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    private val inspectedOriginalTrees = new JavaSet[Tree]()
    private val inspectedTypeTrees = new JavaSet[Tree]()

    private val handleMacroExpansion: Tree => Unit = { original =>
      if (!inspectedOriginalTrees.contains(original)) {
        inspectedOriginalTrees.add(original)
        traverse(original)
      }
    }

    private object PatMatDependencyTraverser extends TypeDependencyTraverser {
      override def addDependency(symbol: global.Symbol): Unit = {
        if (!ignoredSymbol(symbol) && symbol.isSealed) {
          val name = mangledName(symbol)
          if (!isEmptyName(name)) {
            val existingScopes = _currentScopedNamesCache.get(name)
            if (existingScopes == null)
              _currentScopedNamesCache.put(name, PatmatScopes)
            else existingScopes.add(UseScope.PatMatTarget)
          }
        }
        ()
      }
    }

    private object TypeDependencyTraverser extends TypeDependencyTraverser {
      private val ownersCache = new JavaMap[Symbol, JavaSet[Type]]()
      private var nameCache: JavaSet[Name] = _
      private var ownerVisited: Symbol = _

      def setCacheAndOwner(cache: JavaSet[Name], owner: Symbol): Unit = {
        if (ownerVisited != owner) {
          val ts = ownersCache.get(owner)

          if (ts == null) {
            val newVisited = new JavaSet[Type]()
            visited = newVisited
            ownersCache.put(owner, newVisited)
          } else {
            visited = ts
          }

          nameCache = cache
          ownerVisited = owner
        }
      }

      override def addDependency(symbol: global.Symbol): Unit =
        addSymbol(nameCache, symbol)
    }

    private def handleClassicTreeNode(tree: Tree): Unit = tree match {
      // Register names from pattern match target type in PatMatTarget scope
      case matchNode: Match =>
        updateCurrentOwner()
        PatMatDependencyTraverser.traverse(matchNode.selector.tpe)
      case ValDef(mods, _, tpt, _) if mods.isCase && mods.isSynthetic =>
        updateCurrentOwner()
        PatMatDependencyTraverser.traverse(tpt.tpe)
      case _: DefTree | _: Template => ()
      case Import(_, selectors: List[ImportSelector]) =>
        val names = getNamesOfEnclosingScope
        def usedNameInImportSelector(name: Name): Unit = {
          if (!isEmptyName(name) && (name != nme.WILDCARD) && !names.contains(name)) {
            names.add(name)
            ()
          }
        }
        selectors foreach { selector =>
          usedNameInImportSelector(selector.name)
          usedNameInImportSelector(selector.rename)
        }
      /* Original type trees have to be traversed because typer is very
       * aggressive when expanding explicit user-defined types. For instance,
       * `Foo#B` will be expanded to `C` and the dependency on `Foo` will be
       * lost. This makes sure that we traverse all the original prefixes. */
      case t: TypeTree if t.original != null =>
        val original = t.original
        if (!inspectedTypeTrees.contains(original)) {
          inspectedTypeTrees.add(original)
          original.foreach(traverse)
        }

      case t if t.hasSymbolField =>
        val symbol = t.symbol
        if (symbol != rootMirror.RootPackage) {
          addSymbol(getNamesOfEnclosingScope, t.symbol)
        }

        val tpe = t.tpe
        if (!ignoredType(tpe)) {
          // Initialize _currentOwner if it's not
          val cache = getNamesOfEnclosingScope
          TypeDependencyTraverser.setCacheAndOwner(cache, _currentOwner)
          TypeDependencyTraverser.traverse(tpe)
        }
      case l: Literal =>
        processOriginalTreeAttachment(l)(traverse)
      case _ =>
    }

    private var _currentOwner: Symbol = _
    private var _currentNonLocalClass: Symbol = _
    private var _currentNamesCache: JavaSet[Name] = _
    private var _currentScopedNamesCache: JavaMap[Name, EnumSet[UseScope]] = _

    @inline private def resolveNonLocal(from: Symbol): Symbol = {
      val fromClass = enclOrModuleClass(from)
      if (ignoredSymbol(fromClass) || fromClass.hasPackageFlag) NoSymbol
      else localToNonLocalClass.resolveNonLocal(fromClass)
    }

    @inline private def namesInClass(nonLocalClass: Symbol): NamesUsedInClass = {
      if (nonLocalClass == NoSymbol) namesUsedAtTopLevel
      else usedNamesFromClass(ExtractUsedNames.this.className(nonLocalClass))
    }

    /**
     * Updates caches for closest non-local class owner of a tree given
     * `currentOwner`, defined and updated by `Traverser`.
     *
     * This method modifies the state associated with the names variable
     * `_currentNamesCache` and `_currentScopedNamesCache`, which are composed
     * by `_currentOwner` and and `_currentNonLocalClass`.
     *
     * * The used caching strategy works as follows:
     * 1. Do nothing if owners are referentially equal.
     * 2. Otherwise, check if they resolve to the same non-local class.
     *   1. If they do, do nothing
     *   2. Otherwise, overwrite all the pertinent fields to be consistent.
     */
    private def updateCurrentOwner(): Unit = {
      if (_currentOwner == null) {
        // Set the first state for the enclosing non-local class
        _currentOwner = currentOwner
        _currentNonLocalClass = resolveNonLocal(currentOwner)
        val usedInClass = namesInClass(_currentNonLocalClass)
        _currentNamesCache = usedInClass.defaultNames
        _currentScopedNamesCache = usedInClass.scopedNames
      } else if (_currentOwner != currentOwner) {
        val nonLocalClass = resolveNonLocal(currentOwner)
        if (_currentNonLocalClass != nonLocalClass) {
          _currentOwner = currentOwner
          _currentNonLocalClass = nonLocalClass
          val usedInClass = namesInClass(_currentNonLocalClass)
          _currentNamesCache = usedInClass.defaultNames
          _currentScopedNamesCache = usedInClass.scopedNames
        }
      }
    }

    /**
     * Return the names associated with the closest non-local class owner
     * of a tree given `currentOwner`, defined and updated by `Traverser`.
     *
     * This method modifies the state associated with the names variable
     * by calling `updateCurrentOwner()`.
     */
    @inline
    private def getNamesOfEnclosingScope: JavaSet[Name] = {
      updateCurrentOwner()
      _currentNamesCache
    }
  }
}
