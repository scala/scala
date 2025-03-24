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

import java.nio.file.Path
import xsbti.VirtualFile
import xsbti.api.DependencyContext
import DependencyContext._

import scala.tools.nsc.io.{ PlainFile, ZipArchive }
import scala.tools.nsc.Phase

import java.util.{ HashSet => JavaSet }
import java.util.{ HashMap => JavaMap }

object Dependency {
  def name = "xsbt-dependency"
}

/**
 * Extracts dependency information from each compilation unit.
 *
 * This phase detects all the dependencies both at the term and type level.
 *
 * When dependency symbol is processed, it is mapped back to either source file where
 * it's defined in (if it's available in current compilation run) or classpath entry
 * where it originates from. The Symbol -> Classfile mapping is implemented by
 * LocateClassFile that we inherit from.
 */
final class Dependency(val global: CallbackGlobal) extends LocateClassFile with GlobalHelpers {
  import global._

  def newPhase(prev: Phase): Phase = new DependencyPhase(prev)
  private class DependencyPhase(prev: Phase) extends GlobalPhase(prev) {
    override def description = "Extracts dependency information"
    def name = Dependency.name

    override def run(): Unit = {
      val start = System.currentTimeMillis
      super.run()
      callback.dependencyPhaseCompleted()
      val stop = System.currentTimeMillis
      debuglog("Dependency phase took : " + ((stop - start) / 1000.0) + " s")
    }

    // TODO In 2.13, shouldSkipThisPhaseForJava should be overridden instead of cancelled
    // override def shouldSkipThisPhaseForJava = !global.callback.isPickleJava
    override def cancelled(unit: CompilationUnit) = {
      if (Thread.interrupted()) reporter.cancelled = true
      reporter.cancelled || unit.isJava && !global.callback.isPickleJava
    }

    def apply(unit: CompilationUnit): Unit = {
      if (!unit.isJava || global.callback.isPickleJava) {
        // Process dependencies if name hashing is enabled, fail otherwise
        val dependencyProcessor = new DependencyProcessor(unit)
        val dependencyTraverser = new DependencyTraverser(dependencyProcessor)
        // Traverse symbols in compilation unit and register all dependencies
        dependencyTraverser.traverse(unit.body)
      }
    }
  }

  private class DependencyProcessor(unit: CompilationUnit) {
    private def firstClassOrModuleClass(tree: Tree): Option[Symbol] = {
      val maybeClassOrModule = tree find {
        case ((_: ClassDef) | (_: ModuleDef)) => true
        case _                                => false
      }
      maybeClassOrModule.map { classOrModule =>
        val sym = classOrModule.symbol
        if (sym.isModule) sym.moduleClass else sym
      }
    }

    private val sourceFile: VirtualFile = unit.source.file match { case AbstractZincFile(vf) => vf case x => throw new MatchError(x) }
    private val responsibleOfImports = firstClassOrModuleClass(unit.body)
    private var orphanImportsReported = false

    /*
     * Registers top level import dependencies as coming from a first top level
     * class/trait/object declared in the compilation unit. Otherwise, issue warning.
     */
    def processTopLevelImportDependency(dep: Symbol): Unit = {
      if (!orphanImportsReported) {
        responsibleOfImports match {
          case Some(classOrModuleDef) =>
            memberRef(ClassDependency(classOrModuleDef, dep))
          case None =>
            reporter.echo(
              unit.position(0),
              Feedback.OrphanTopLevelImports
            ) // package-info.java & empty scala files
            orphanImportsReported = true
        }
      }
      ()
    }

    // Define processor reusing `processDependency` definition
    val memberRef = processDependency(DependencyByMemberRef, allowLocal = false)(_)
    val inheritance = processDependency(DependencyByInheritance, allowLocal = true)(_)
    val localInheritance = processDependency(LocalDependencyByInheritance, allowLocal = true)(_)

    @deprecated("Use processDependency that takes allowLocal.", "1.1.0")
    def processDependency(context: DependencyContext)(dep: ClassDependency): Unit =
      processDependency(context, allowLocal = true)(dep)

    /*
     * Handles dependency on given symbol by trying to figure out if represents a term
     * that is coming from either source code (not necessarily compiled in this compilation
     * run) or from class file and calls respective callback method.
     */
    def processDependency(context: DependencyContext, allowLocal: Boolean)(
        dep: ClassDependency
    ): Unit = {
      val fromClassName = classNameAsString(dep.from)

      def binaryDependency(file: Path, binaryClassName: String) = {
        callback.binaryDependency(file, binaryClassName, fromClassName, sourceFile, context)
      }
      import scala.tools.nsc.io.AbstractFile
      def processExternalDependency(binaryClassName: String, at: AbstractFile): Unit = {
        at match {
          case zipEntry: ZipArchive#Entry =>
            // The dependency comes from a JAR
            for {
              zip <- zipEntry.underlyingSource
            } {
              // workaround for JDK9 and Scala 2.10/2.11, see https://github.com/sbt/sbt/pull/3701
              val ignore = zip.file == null || (!zip.hasExtension("jar") && zip.isDirectory)
              if (!ignore)
                binaryDependency(zip.file.toPath, binaryClassName)
            }
          case pf: ZincCompat.PlainNioFile =>
            // The dependency comes from a class file
            binaryDependency(ZincCompat.unwrapPlainNioFile(pf), binaryClassName)
          case pf: PlainFile =>
            // The dependency comes from a class file
            binaryDependency(pf.file.toPath, binaryClassName)
          case _ =>
          // On Scala 2.10 you get Internal error: <none> comes from unknown origin null
          // if you uncomment the following:
          // reporter.error(
          //   NoPosition,
          //   s"Internal error: ${binaryClassName} comes from unknown origin ${at} (${at.getClass})"
          // )
        }
      }

      val targetSymbol = dep.to
      val onSource = targetSymbol.sourceFile
      onSource match {
        case AbstractZincFile(onSourceFile) =>
          if (onSourceFile != sourceFile || allowLocal) {
            // We cannot ignore dependencies coming from the same source file because
            // the dependency info needs to propagate. See source-dependencies/trait-trait-211.
            val onClassName = classNameAsString(dep.to)
            callback.classDependency(onClassName, fromClassName, context)
          } else ()
        // This could match null or scala.reflect.io.FileZipArchive$LeakyEntry
        case _ =>
          val noByteCode = (
            // Ignore packages right away as they don't map to a class file/jar
            targetSymbol.hasFlag(scala.tools.nsc.symtab.Flags.PACKAGE) ||
              // Seen in the wild: an Ident as the original of a TypeTree from a synthetic case accessor was symbol-less
              targetSymbol == NoSymbol ||
              // Also ignore magic symbols that don't have bytecode like Any/Nothing/Singleton/<byname>/<repeated>/...
              isSyntheticCoreClass(targetSymbol)
          )
          if (!noByteCode) {
            classFile(targetSymbol) match {
              case Some((at, binaryClassName)) =>
                // Associated file is set, so we know which classpath entry it came from
                processExternalDependency(binaryClassName, at)
              case None =>
                /* If there is no associated file, it's likely the compiler didn't set it correctly.
                 * This happens very rarely, see https://github.com/sbt/zinc/issues/559 as an example,
                 * but when it does we must ensure the incremental compiler tries its best no to lose
                 * any dependency. Therefore, we do a last-time effort to get the origin of the symbol
                 * by inspecting the classpath manually.
                 */
                val fqn = fullName(targetSymbol, '.', targetSymbol.moduleSuffix, includePackageObjectClassNames = false)
                global.findAssociatedFile(fqn) match {
                  case Some((at, true)) =>
                    processExternalDependency(fqn, at)
                  case Some((_, false)) | None =>
                    // Study the possibility of warning or adding this to the zinc profiler so that
                    // if users reports errors, the lost dependencies are present in the zinc profiler
                    debuglog(Feedback.noOriginFileForExternalSymbol(targetSymbol))
                }
            }
          }
      }
    }
  }

  private case class ClassDependency(from: Symbol, to: Symbol)

  private final class DependencyTraverser(processor: DependencyProcessor) extends Traverser {
    // are we traversing an Import node at the moment?
    private var inImportNode = false

    // Define caches for dependencies that have already been processed
    private val _memberRefCache = new JavaSet[ClassDependency]()
    private val _inheritanceCache = new JavaSet[ClassDependency]()
    private val _localInheritanceCache = new JavaSet[ClassDependency]()
    private val _topLevelImportCache = new JavaSet[Symbol]()

    private var _currentDependencySource: Symbol = _
    private var _currentNonLocalClass: Symbol = _
    private var _isLocalSource: Boolean = false

    @inline def resolveNonLocalClass(from: Symbol): (Symbol, Boolean) = {
      val fromClass = enclOrModuleClass(from)
      if (fromClass == NoSymbol || fromClass.hasPackageFlag) (fromClass, false)
      else {
        val nonLocal = localToNonLocalClass.resolveNonLocal(fromClass)
        (nonLocal, fromClass != nonLocal)
      }
    }

    /**
     * Resolves dependency source (that is, the closest non-local enclosing
     * class from a given `currentOwner` set by the `Traverser`).
     *
     * This method modifies the value of `_currentDependencySource`,
     * `_currentNonLocalClass` and `_isLocalSource` and it is not modeled
     * as a case class for performance reasons.
     *
     * The used caching strategy works as follows:
     * 1. Return previous non-local class if owners are referentially equal.
     * 2. Otherwise, check if they resolve to the same non-local class.
     *   1. If they do, overwrite `_isLocalSource` and return
     *        `_currentNonLocalClass`.
     *   2. Otherwise, overwrite all the pertinent fields to be consistent.
     */
    private def resolveDependencySource: Symbol = {
      if (_currentDependencySource == null) {
        // First time we access it, initialize it
        _currentDependencySource = currentOwner
        val (nonLocalClass, isLocal) = resolveNonLocalClass(currentOwner)
        _currentNonLocalClass = nonLocalClass
        _isLocalSource = isLocal
        nonLocalClass
      } else {
        // Check if cached is equally referential
        if (_currentDependencySource == currentOwner) _currentNonLocalClass
        else {
          // Check they resolve to the same nonLocalClass. If so, spare writes.
          val (nonLocalClass, isLocal) = resolveNonLocalClass(currentOwner)
          if (_currentNonLocalClass == nonLocalClass) {
            // Resolution can be the same, but the origin affects `isLocal`
            _isLocalSource = isLocal
            _currentNonLocalClass
          } else {
            _currentDependencySource = _currentDependencySource
            _currentNonLocalClass = nonLocalClass
            _isLocalSource = isLocal
            _currentNonLocalClass
          }
        }
      }
    }

    /**
     * Process a given ClassDependency and add it to the cache.
     *
     * This class dependency can be of three different types:
     *   1. Member reference;
     *   2. Local inheritance; or,
     *   3. Inheritance.
     */
    private def addClassDependency(
        cache: JavaSet[ClassDependency],
        process: ClassDependency => Unit,
        fromClass: Symbol,
        dep: Symbol
    ): Unit = {
      assert(fromClass.isClass, Feedback.expectedClassSymbol(fromClass))
      val depClass = enclOrModuleClass(dep)
      val dependency = ClassDependency(fromClass, depClass)
      if (
        !cache.contains(dependency) &&
        !depClass.isRefinementClass
      ) {
        process(dependency)
        cache.add(dependency)
        ()
      }
    }

    def addTopLevelImportDependency(dep: global.Symbol): Unit = {
      val depClass = enclOrModuleClass(dep)
      if (!_topLevelImportCache.contains(depClass) && !dep.hasPackageFlag) {
        processor.processTopLevelImportDependency(depClass)
        _topLevelImportCache.add(depClass)
        ()
      }
    }

    private def addTreeDependency(tree: Tree): Unit = {
      addDependency(tree.symbol)
      val tpe = tree.tpe
      if (!ignoredType(tpe)) {
        addTypeDependencies(tpe)
      }
      ()
    }

    private def addDependency(dep: Symbol): Unit = {
      val fromClass = resolveDependencySource
      if (ignoredSymbol(fromClass) || fromClass.hasPackageFlag) {
        if (inImportNode) addTopLevelImportDependency(dep)
        else devWarning(Feedback.missingEnclosingClass(dep, currentOwner))
      } else {
        addClassDependency(_memberRefCache, processor.memberRef, fromClass, dep)
      }
    }

    /** Define a type traverser to keep track of the type dependencies. */
    object TypeDependencyTraverser extends TypeDependencyTraverser {
      type Handler = Symbol => Unit
      // Type dependencies are always added to member references
      val memberRefHandler = processor.memberRef
      def createHandler(fromClass: Symbol): Handler = { (dep: Symbol) =>
        if (ignoredSymbol(fromClass) || fromClass.hasPackageFlag) {
          if (inImportNode) addTopLevelImportDependency(dep)
          else devWarning(Feedback.missingEnclosingClass(dep, currentOwner))
        } else {
          addClassDependency(_memberRefCache, memberRefHandler, fromClass, dep)
        }
      }

      val cache = new JavaMap[Symbol, (Handler, JavaSet[Type])]()
      private var handler: Handler = _
      private var visitedOwner: Symbol = _
      def setOwner(owner: Symbol) = {
        if (visitedOwner != owner) {
          cache.get(owner) match {
            case null =>
              val newVisited = new JavaSet[Type]()
              handler = createHandler(owner)
              cache.put(owner, handler -> newVisited)
              visited = newVisited
              visitedOwner = owner
            case (h, ts) =>
              visited = ts
              handler = h
          }
        }
      }

      override def addDependency(symbol: global.Symbol) = handler(symbol)
    }

    def addTypeDependencies(tpe: Type): Unit = {
      val fromClass = resolveDependencySource
      TypeDependencyTraverser.setOwner(fromClass)
      TypeDependencyTraverser.traverse(tpe)
    }

    private def addInheritanceDependency(dep: Symbol): Unit = {
      val fromClass = resolveDependencySource
      if (_isLocalSource) {
        addClassDependency(_localInheritanceCache, processor.localInheritance, fromClass, dep)
      } else {
        addClassDependency(_inheritanceCache, processor.inheritance, fromClass, dep)
      }
    }

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    private val inspectedOriginalTrees = new JavaSet[Tree]()

    override def traverse(tree: Tree): Unit = tree match {
      case Import(expr, selectors) =>
        inImportNode = true
        traverse(expr)
        selectors.foreach {
          case ImportSelector(nme.WILDCARD, _, null, _) =>
          // in case of wildcard import we do not rely on any particular name being defined
          // on `expr`; all symbols that are being used will get caught through selections
          case ImportSelector(name: Name, _, _, _) =>
            def lookupImported(name: Name) = expr.symbol.info.member(name)
            // importing a name means importing both a term and a type (if they exist)
            val termSymbol = lookupImported(name.toTermName)
            if (termSymbol.info != NoType) addDependency(termSymbol)
            addDependency(lookupImported(name.toTypeName))
        }
        inImportNode = false
      /*
       * Idents are used in number of situations:
       *  - to refer to local variable
       *  - to refer to a top-level package (other packages are nested selections)
       *  - to refer to a term defined in the same package as an enclosing class;
       *    this looks fishy, see this thread:
       *    https://groups.google.com/d/topic/scala-internals/Ms9WUAtokLo/discussion
       */
      case id: Ident => addTreeDependency(id)
      case sel @ Select(qual, _) =>
        traverse(qual); addTreeDependency(sel)
      case sel @ SelectFromTypeTree(qual, _) =>
        traverse(qual); addTreeDependency(sel)

      case Template(parents, self, body) =>
        // use typeSymbol to dealias type aliases -- we want to track the dependency on the real class in the alias's RHS
        def flattenTypeToSymbols(tp: Type): List[Symbol] =
          if (tp eq null) Nil
          else
            tp match {
              // rt.typeSymbol is redundant if we list out all parents, TODO: what about rt.decls?
              case rt: RefinedType => rt.parents.flatMap(flattenTypeToSymbols)
              case _               => List(tp.typeSymbol)
            }

        val inheritanceTypes = parents.map(_.tpe).toSet
        val inheritanceSymbols = inheritanceTypes.flatMap(flattenTypeToSymbols)

        debuglog(
          "Parent types for " + tree.symbol + " (self: " + self.tpt.tpe + "): " + inheritanceTypes + " with symbols " + inheritanceSymbols
            .map(_.fullName)
        )

        inheritanceSymbols.foreach { symbol =>
          addInheritanceDependency(symbol)
          addDependency(symbol)
        }

        inheritanceTypes.foreach(addTypeDependencies)
        addTypeDependencies(self.tpt.tpe)

        traverseTrees(body)

      case Literal(value) if value.tag == ClazzTag =>
        addTypeDependencies(value.typeValue)

      /* Original type trees have to be traversed because typer is very
       * aggressive when expanding explicit user-defined types. For instance,
       * `Foo#B` will be expanded to `C` and the dependency on `Foo` will be
       * lost. This makes sure that we traverse all the original prefixes. */
      case typeTree: TypeTree if !ignoredType(typeTree.tpe) =>
        val original = typeTree.original
        if (original != null && !inspectedOriginalTrees.contains(original)) {
          traverse(original)
          inspectedOriginalTrees.add(original)
        }
        addTypeDependencies(typeTree.tpe)

      case m @ MacroExpansionOf(original) if inspectedOriginalTrees.add(original) =>
        traverse(original)
        super.traverse(m)

      case l: Literal =>
        processOriginalTreeAttachment(l)(traverse)
        super.traverse(l)

      case _: ClassDef | _: ModuleDef if !ignoredSymbol(tree.symbol) =>
        // make sure we cache lookups for all classes declared in the compilation unit; the recorded information
        // will be used in Analyzer phase
        val sym = if (tree.symbol.isModule) tree.symbol.moduleClass else tree.symbol
        localToNonLocalClass.resolveNonLocal(sym)
        super.traverse(tree)

      case f: Function =>
        f.attachments.get[SAMFunction].foreach { sam =>
          addDependency(sam.samTp.typeSymbol)
          // Not using addInheritanceDependency as it would incorrectly classify dependency as non-local
          // ref: https://github.com/scala/scala/pull/10617/files#r1415226169
          val from = resolveDependencySource
          addClassDependency(_localInheritanceCache, processor.localInheritance, from, sam.samTp.typeSymbol)
        }
        super.traverse(tree)

      case other => super.traverse(other)
    }
  }
}
