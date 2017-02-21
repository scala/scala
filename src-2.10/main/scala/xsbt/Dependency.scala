/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import java.io.File

import xsbti.api.DependencyContext
import DependencyContext._

import scala.tools.nsc.io.{ PlainFile, ZipArchive }
import scala.tools.nsc.Phase

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

    def apply(unit: CompilationUnit): Unit = {
      if (!unit.isJava) {
        // Process dependencies if name hashing is enabled, fail otherwise
        if (global.callback.nameHashing) {
          val dependencyProcessor = new DependencyProcessor(unit)
          val dependencyTraverser = new DependencyTraverser(dependencyProcessor)
          // Traverse symbols in compilation unit and register all dependencies
          dependencyTraverser.traverse(unit.body)
        } else {
          throw new UnsupportedOperationException(Feedback.NameHashingDisabled)
        }
      }
    }
  }

  private class DependencyProcessor(unit: CompilationUnit) {
    private def firstClassOrModuleClass(tree: Tree): Option[Symbol] = {
      tree foreach {
        case classOrModule @ ((_: ClassDef) | (_: ModuleDef)) =>
          val sym = classOrModule.symbol
          return Some(if (sym.isModule) sym.moduleClass else sym)
        case _ => ()
      }
      None
    }

    private val sourceFile = unit.source.file.file
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
            reporter.warning(unit.position(0), Feedback.OrphanTopLevelImports)
            orphanImportsReported = true
        }
      }
      ()
    }

    // Define processor reusing `processDependency` definition
    val memberRef = processDependency(DependencyByMemberRef) _
    val inheritance = processDependency(DependencyByInheritance) _
    val localInheritance = processDependency(LocalDependencyByInheritance) _

    /*
     * Handles dependency on given symbol by trying to figure out if represents a term
     * that is coming from either source code (not necessarily compiled in this compilation
     * run) or from class file and calls respective callback method.
     */
    def processDependency(context: DependencyContext)(dep: ClassDependency): Unit = {
      val fromClassName = classNameAsString(dep.from)

      def binaryDependency(file: File, binaryClassName: String) =
        callback.binaryDependency(file, binaryClassName, fromClassName, sourceFile, context)

      import scala.tools.nsc.io.AbstractFile
      def processExternalDependency(binaryClassName: String, at: AbstractFile) = {
        at match {
          case zipEntry: ZipArchive#Entry =>
            // The dependency comes from a JAR
            for {
              zip <- zipEntry.underlyingSource
              classFile <- Option(zip.file)
            } binaryDependency(classFile, binaryClassName)
          case pf: PlainFile =>
            // The dependency comes from a class file
            binaryDependency(pf.file, binaryClassName)
          case _ =>
          // TODO: If this happens, scala internals have changed. Log error.
        }
      }

      val onSource = dep.to.sourceFile
      if (onSource == null) {
        // Dependency is external -- source is undefined
        classFile(dep.to) match {
          case Some((at, binaryClassName)) =>
            processExternalDependency(binaryClassName, at)
          case None =>
            debuglog(Feedback.noOriginFileForExternalSymbol(dep.to))
        }
      } else if (onSource.file != sourceFile) {
        // Dependency is internal -- but from other file / compilation unit
        val onClassName = classNameAsString(dep.to)
        callback.classDependency(onClassName, fromClassName, context)
      } else () // Comes from the same file, ignore
    }
  }

  private case class ClassDependency(from: Symbol, to: Symbol)

  private class DependencyTraverser(processor: DependencyProcessor) extends Traverser {
    // are we traversing an Import node at the moment?
    private var inImportNode = false

    // Define caches for dependencies that have already been processed
    import scala.collection.mutable.HashSet
    private val _memberRefCache = HashSet.empty[ClassDependency]
    private val _inheritanceCache = HashSet.empty[ClassDependency]
    private val _localInheritanceCache = HashSet.empty[ClassDependency]
    private val _topLevelImportCache = HashSet.empty[Symbol]

    /** Return the enclosing class or the module class if it's a module. */
    private def enclOrModuleClass(s: Symbol): Symbol =
      if (s.isModule) s.moduleClass else s.enclClass

    case class DependencySource(owner: Symbol) {
      val (fromClass: Symbol, isLocal: Boolean) = {
        val fromClass = enclOrModuleClass(owner)
        if (fromClass == NoSymbol || fromClass.hasPackageFlag)
          (fromClass, false)
        else {
          val fromNonLocalClass = localToNonLocalClass.resolveNonLocal(fromClass)
          assert(!(fromClass == NoSymbol || fromClass.hasPackageFlag))
          (fromNonLocalClass, fromClass != fromNonLocalClass)
        }
      }
    }

    private var _currentDependencySource: DependencySource = null

    /**
     * Resolves dependency source by getting the enclosing class for `currentOwner`
     * and then looking up the most inner enclosing class that is non local.
     * The second returned value indicates if the enclosing class for `currentOwner`
     * is a local class.
     */
    private def resolveDependencySource(): DependencySource = {
      def newOne(): DependencySource = {
        val fresh = DependencySource(currentOwner)
        _currentDependencySource = fresh
        _currentDependencySource
      }
      _currentDependencySource match {
        case null => newOne()
        case cached if currentOwner == cached.owner =>
          cached
        case _ => newOne()
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
      cache: HashSet[ClassDependency],
      process: ClassDependency => Unit,
      fromClass: Symbol,
      dep: Symbol
    ): Unit = {
      assert(fromClass.isClass, Feedback.expectedClassSymbol(fromClass))
      val depClass = enclOrModuleClass(dep)
      val dependency = ClassDependency(fromClass, depClass)
      if (!cache.contains(dependency) &&
        fromClass.associatedFile != depClass.associatedFile &&
        !depClass.isRefinementClass) {
        process(dependency)
        cache += dependency
        ()
      }
    }

    def addTopLevelImportDependency(dep: global.Symbol): Unit = {
      val depClass = enclOrModuleClass(dep)
      if (!_topLevelImportCache.contains(depClass) && !dep.hasPackageFlag) {
        processor.processTopLevelImportDependency(depClass)
        _topLevelImportCache += depClass
        ()
      }
    }

    private def addTreeDependency(tree: Tree): Unit = {
      addDependency(tree.symbol)
      val tpe = tree.tpe
      if (!ignoredType(tpe))
        foreachNotPackageSymbolInType(tpe)(addDependency)
      ()
    }

    private def addDependency(dep: Symbol): Unit = {
      val fromClass = resolveDependencySource().fromClass
      if (ignoredSymbol(fromClass) || fromClass.hasPackageFlag) {
        if (inImportNode) addTopLevelImportDependency(dep)
        else debugwarn(Feedback.missingEnclosingClass(dep, currentOwner))
      } else {
        addClassDependency(_memberRefCache, processor.memberRef, fromClass, dep)
      }
    }

    private def addInheritanceDependency(dep: Symbol): Unit = {
      val dependencySource = resolveDependencySource()
      val fromClass = dependencySource.fromClass
      if (dependencySource.isLocal) {
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
    private val inspectedOriginalTrees = collection.mutable.Set.empty[Tree]

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
            addDependency(lookupImported(name.toTermName))
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
        def flattenTypeToSymbols(tp: Type): List[Symbol] = if (tp eq null) Nil
        else tp match {
          // rt.typeSymbol is redundant if we list out all parents, TODO: what about rt.decls?
          case rt: RefinedType => rt.parents.flatMap(flattenTypeToSymbols)
          case _               => List(tp.typeSymbol)
        }

        val inheritanceTypes = parents.map(_.tpe).toSet
        val inheritanceSymbols = inheritanceTypes.flatMap(flattenTypeToSymbols)

        debuglog("Parent types for " + tree.symbol + " (self: " + self.tpt.tpe + "): " + inheritanceTypes + " with symbols " + inheritanceSymbols.map(_.fullName))

        inheritanceSymbols.foreach(addSymbolFromParent)
        inheritanceTypes.foreach(addSymbolsFromType)
        addSymbolsFromType(self.tpt.tpe)

        traverseTrees(body)

      // In some cases (eg. macro annotations), `typeTree.tpe` may be null. See sbt/sbt#1593 and sbt/sbt#1655.
      case typeTree: TypeTree if !ignoredType(typeTree.tpe) =>
        foreachNotPackageSymbolInType(typeTree.tpe)(addDependency)
      case m @ MacroExpansionOf(original) if inspectedOriginalTrees.add(original) =>
        traverse(original)
        super.traverse(m)
      case _: ClassDef | _: ModuleDef if !ignoredSymbol(tree.symbol) =>
        // make sure we cache lookups for all classes declared in the compilation unit; the recorded information
        // will be used in Analyzer phase
        val sym = if (tree.symbol.isModule) tree.symbol.moduleClass else tree.symbol
        localToNonLocalClass.resolveNonLocal(sym)
        super.traverse(tree)
      case other => super.traverse(other)
    }

    val addSymbolFromParent: Symbol => Unit = { symbol =>
      addInheritanceDependency(symbol)
      addDependency(symbol)
    }
    val addSymbolsFromType: Type => Unit = { tpe =>
      foreachNotPackageSymbolInType(tpe)(addDependency)
    }
  }
}
