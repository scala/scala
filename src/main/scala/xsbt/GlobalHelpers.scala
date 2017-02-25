/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import scala.tools.nsc.Global

trait GlobalHelpers {
  val global: Global
  import global._

  /** Return true if type shall be ignored, false otherwise. */
  @inline def ignoredType(tpe: Type) = {
    tpe == null ||
      tpe == NoType ||
      tpe.typeSymbol == EmptyPackageClass
  }

  /** Return true if symbol shall be ignored, false otherwise. */
  @inline def ignoredSymbol(symbol: Symbol) = {
    symbol == null ||
      symbol == NoSymbol ||
      symbol == EmptyPackageClass
  }

  /** Return true if name is empty, false otherwise. */
  def isEmptyName(name: Name): Boolean = {
    name match {
      case null | nme.EMPTY | nme.EMPTY_PACKAGE_NAME |
        tpnme.EMPTY | tpnme.EMPTY_PACKAGE_NAME => true
      case _ => false
    }
  }

  /** Apply `op` on every type symbol which doesn't represent a package. */
  def foreachNotPackageSymbolInType(tpe: Type)(op: Symbol => Unit): Unit = {
    new ForEachTypeTraverser(_ match {
      case null =>
      case tpe =>
        val sym = tpe.typeSymbolDirect
        if (sym != NoSymbol && !sym.hasPackageFlag) op(sym)
    }).traverse(tpe)
  }

  private[xsbt] class TypeDependencyTraverser(addDependency: Symbol => Unit)
    extends TypeTraverser {

    /** Add type dependency ignoring packages and inheritance info from classes. */
    @inline private def addTypeSymbolDependency(symbol: Symbol): Unit = {
      addDependency(symbol)
      if (!symbol.isClass) {
        traverse(symbol.info)
      }
    }

    /** Add type dependency *AND* traverse prefix iff is not a package. */
    @inline private def addTypeDependency(tpe: Type): Unit = {
      val symbol = tpe.typeSymbolDirect
      if (!symbol.hasPackageFlag) {
        addTypeSymbolDependency(symbol)
        traverse(tpe.prefix)
      }
    }

    // Define cache and populate it with known types at initialization time
    private val visited = scala.collection.mutable.HashSet.empty[Type]

    /** Clear the cache after every `traverse` invocation at the call-site. */
    private[xsbt] def reinitializeVisited(): Unit = visited.clear()

    /**
     * Traverse the type and its info to track all type dependencies.
     *
     * Note that tpe cannot be either `NoSymbol` or `null`.
     * Check that you don't pass those types at the call-site.
     */
    override def traverse(tpe: Type): Unit = {
      if ((tpe ne NoType) && !visited.contains(tpe)) {
        visited += tpe
        tpe match {
          case singleRef: SingleType =>
            addTypeDependency(singleRef)

          case typeRef: TypeRef =>
            // Traverse materialized type arguments
            typeRef.typeArguments.foreach(traverse)
            addTypeDependency(typeRef)

          case MethodType(_, _) =>
            // Traverse the types of method parameters definitions
            tpe.params.foreach(param => traverse(param.tpe))
            // Traverse return type
            traverse(tpe.resultType)

          case PolyType(_, _) =>
            // Traverse the symbols of poly types and their prefixes
            tpe.typeParams.foreach { typeParam =>
              addTypeSymbolDependency(typeParam)
              val prefix = typeParam.info.prefix
              if (!prefix.typeSymbolDirect.hasPackageFlag)
                traverse(prefix)
            }
            // Traverse return type
            traverse(tpe.resultType)

          case TypeBounds(lo, hi) =>
            // Ignore default types for lo and hi bounds
            if (!(lo == definitions.NothingTpe)) traverse(lo)
            if (!(hi == definitions.AnyTpe)) traverse(hi)

          case RefinedType(parents, decls) =>
            parents.foreach(traverse)
            decls.toIterator.foreach { decl =>
              if (decl.isType) addTypeSymbolDependency(decl)
              else addDependency(decl)
            }

          case ExistentialType(quantified, underlying) =>
            quantified.foreach(quantified => traverse(quantified.tpe))
            traverse(underlying)

          case ThisType(_) | ConstantType(_) =>
            traverse(tpe.underlying)

          case _ =>
            mapOver(tpe)
            ()
        }
      }
    }
  }

  /** Returns true if given tree contains macro attchment. In such case calls func on tree from attachment. */
  def processMacroExpansion(in: Tree)(func: Tree => Unit): Boolean = {
    // Hotspot
    var seen = false
    in.attachments.all.foreach {
      case _ if seen =>
      case macroAttachment: analyzer.MacroExpansionAttachment =>
        func(macroAttachment.expandee)
        seen = true
      case _ =>
    }
    seen
  }

  object MacroExpansionOf {
    def unapply(tree: Tree): Option[Tree] = {
      tree.attachments.all.collect {
        case att: analyzer.MacroExpansionAttachment => att.expandee
      }.headOption
    }
  }

  /** Return the enclosing class or the module class if it's a module. */
  def enclOrModuleClass(s: Symbol): Symbol =
    if (s.isModule) s.moduleClass else s.enclClass

  /** Define common error messages for error reporting and assertions. */
  object Feedback {
    val NameHashingDisabled = "Turning off name hashing is not supported in class-based dependency trackging."
    val OrphanTopLevelImports = noTopLevelMember("top level imports")
    val OrphanNames = noTopLevelMember("names")

    def noOriginFileForExternalSymbol(symbol: Symbol) =
      s"The symbol $symbol comes from an unknown source or compiled source -- ignoring."
    def expectedClassSymbol(culprit: Symbol): String =
      s"The ${culprit.fullName} defined at ${culprit.fullLocationString} is not a class symbol."
    def missingEnclosingClass(culprit: Symbol, owner: Symbol): String =
      s"No enclosing class. Discarding dependency on $culprit (currentOwner = $owner)."
    def noTopLevelMember(found: String) = s"""
      |Found $found but no class, trait or object is defined in the compilation unit.
      |The incremental compiler cannot record the dependency information in such case.
      |Some errors like unused import referring to a non-existent class might not be reported.
    """.stripMargin
  }
}
