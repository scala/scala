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
class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType) extends Compat with ClassName with GlobalHelpers {
  import global._

  def extract(unit: CompilationUnit): Iterable[(String, Iterable[String])] = {
    val tree = unit.body
    val traverser = new ExtractUsedNamesTraverser
    traverser.traverse(tree)
    val namesUsedAtTopLevel = traverser.namesUsedAtTopLevel

    if (namesUsedAtTopLevel.nonEmpty) {
      val classOrModuleDef = firstClassOrModuleDef(tree)
      classOrModuleDef match {
        case Some(classOrModuleDef) =>
          val sym = classOrModuleDef.symbol
          val firstClassSymbol = if (sym.isModule) sym.moduleClass else sym
          val firstClassName = className(firstClassSymbol)
          traverser.usedNamesFromClass(firstClassName) ++= namesUsedAtTopLevel
        case None =>
          reporter.warning(unit.position(0), Feedback.OrphanNames)
      }
    }

    traverser.usedNamesFromClasses.map { tpl =>
      // NOTE: We don't decode the full class name, only dependent names.
      tpl._1.toString.trim -> tpl._2.map(_.decode.trim)
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
    val usedNamesFromClasses = mutable.Map.empty[Name, mutable.Set[Name]]
    val namesUsedAtTopLevel = mutable.Set.empty[Name]

    override def traverse(tree: Tree): Unit = {
      handleClassicTreeNode(tree)
      processMacroExpansion(tree)(handleMacroExpansion)
      super.traverse(tree)
    }

    val addSymbol: Symbol => Unit = {
      symbol =>
        val enclosingNonLocalClass = resolveEnclosingNonLocalClass
        if (!ignoredSymbol(symbol)) {
          val name = symbol.name
          // Synthetic names are no longer included. See https://github.com/sbt/sbt/issues/2537
          if (!isEmptyName(name) && !enclosingNonLocalClass.containsName(name))
            enclosingNonLocalClass.addName(name)
          ()
        }
    }

    /** Returns mutable set with all names from given class used in current context */
    def usedNamesFromClass(className: Name): collection.mutable.Set[Name] = {
      usedNamesFromClasses.get(className) match {
        case Some(setForClass) => setForClass
        case None =>
          val emptySet = scala.collection.mutable.Set.empty[Name]
          usedNamesFromClasses.put(className, emptySet)
          emptySet
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

    private val handleMacroExpansion: Tree => Unit = { original =>
      if (!inspectedOriginalTrees.contains(original)) {
        inspectedOriginalTrees += original
        traverse(original)
      }
    }

    object TypeDependencyTraverser extends TypeDependencyTraverser(addSymbol)

    private def handleClassicTreeNode(tree: Tree): Unit = tree match {
      case _: DefTree | _: Template => ()
      case Import(_, selectors: List[ImportSelector]) =>
        val enclosingNonLocalClass = resolveEnclosingNonLocalClass()
        def usedNameInImportSelector(name: Name): Unit = {
          if (!isEmptyName(name) && (name != nme.WILDCARD) &&
            !enclosingNonLocalClass.containsName(name)) {
            enclosingNonLocalClass.addName(name)
          }
        }
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
        val original = t.original
        if (!inspectedTypeTrees.contains(original)) {
          inspectedTypeTrees += original
          original.foreach(traverse)
        }
      case t if t.hasSymbol =>
        val symbol = t.symbol
        if (symbol != rootMirror.RootPackage)
          addSymbol(t.symbol)
        val tpe = t.tpe
        if (!ignoredType(tpe)) {
          TypeDependencyTraverser.traverse(tpe)
          TypeDependencyTraverser.reinitializeVisited()
        }
      case _ =>
    }

    private case class EnclosingNonLocalClass(currentOwner: Symbol) {
      private val nonLocalClass: Symbol = {
        val fromClass = enclOrModuleClass(currentOwner)
        if (ignoredSymbol(fromClass) || fromClass.hasPackageFlag) null
        else localToNonLocalClass.resolveNonLocal(fromClass)
      }

      private val usedNamesSet: collection.mutable.Set[Name] = {
        if (nonLocalClass == null) namesUsedAtTopLevel
        else usedNamesFromClass(ExtractUsedNames.this.className(nonLocalClass))
      }

      def addName(name: Name): Unit = {
        usedNamesSet += name
        ()
      }

      def containsName(name: Name): Boolean =
        usedNamesSet.contains(name)
    }

    private var _lastEnclosingNonLocalClass: EnclosingNonLocalClass = null

    /**
     * Resolves a class to which we attribute a used name by getting the enclosing class
     * for `currentOwner` and then looking up the most inner enclosing class that is non local.
     * The second returned value indicates if the enclosing class for `currentOwner`
     * is a local class.
     */
    private def resolveEnclosingNonLocalClass(): EnclosingNonLocalClass = {
      /* Note that `currentOwner` is set by Global and points to the owner of
       * the tree that we traverse. Therefore, it's not ensured to be a non local
       * class. The non local class is resolved inside `EnclosingNonLocalClass`. */
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
    !ignoredSymbol(symbol) && !isEmptyName(symbol.name)
  }
}
