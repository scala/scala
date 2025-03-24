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

import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags
import xsbti.api._
import xsbti.VirtualFile

object API {
  val name = "xsbt-api"
}

final class API(val global: CallbackGlobal) extends Compat with GlobalHelpers with ClassName {
  import global._

  import scala.collection.mutable
  private val nonLocalClassSymbolsInCurrentUnits = new mutable.HashSet[Symbol]()

  def newPhase(prev: Phase) = new ApiPhase(prev)
  class ApiPhase(prev: Phase) extends GlobalPhase(prev) {
    override def description = "Extracts the public API from source files."
    def name = API.name
    override def run(): Unit = {
      val start = System.currentTimeMillis
      super.run()

      // After processing all units, register generated classes
      registerGeneratedClasses(nonLocalClassSymbolsInCurrentUnits.iterator)
      nonLocalClassSymbolsInCurrentUnits.clear()

      callback.apiPhaseCompleted()
      val stop = System.currentTimeMillis
      debuglog("API phase took : " + ((stop - start) / 1000.0) + " s")
    }

    // TODO In 2.13, shouldSkipThisPhaseForJava should be overridden instead of cancelled
    // override def shouldSkipThisPhaseForJava = !global.callback.isPickleJava
    override def cancelled(unit: CompilationUnit) = {
      if (Thread.interrupted()) reporter.cancelled = true
      reporter.cancelled || unit.isJava && !global.callback.isPickleJava
    }

    def apply(unit: global.CompilationUnit): Unit = processUnit(unit)

    private def processUnit(unit: CompilationUnit): Unit = {
      if (!unit.isJava || global.callback.isPickleJava) {
        processScalaUnit(unit)
      }
    }

    private def processScalaUnit(unit: CompilationUnit): Unit = {
      val sourceFile: VirtualFile = unit.source.file match { case AbstractZincFile(vf) => vf case x => throw new MatchError(x) }
      debuglog("Traversing " + sourceFile)
      callback.startSource(sourceFile)
      val extractApi = new ExtractAPI[global.type](global, sourceFile)
      val traverser = new TopLevelHandler(extractApi)
      traverser.apply(unit.body)

      val extractUsedNames = new ExtractUsedNames[global.type](global)
      extractUsedNames.extractAndReport(unit)

      val classApis = traverser.allNonLocalClasses
      val mainClasses = traverser.mainClasses

      // Use of iterators make this code easier to profile

      val classApisIt = classApis.iterator
      while (classApisIt.hasNext) {
        callback.api(sourceFile, classApisIt.next())
      }

      val mainClassesIt = mainClasses.iterator
      while (mainClassesIt.hasNext) {
        callback.mainClass(sourceFile, mainClassesIt.next())
      }

      extractApi.allExtractedNonLocalSymbols.foreach { cs =>
        // Only add the class symbols defined in this compilation unit
        if (cs.sourceFile != null) nonLocalClassSymbolsInCurrentUnits.+=(cs)
      }
    }
  }

  private case class FlattenedNames(binaryName: String, className: String)

  /**
   * Registers only non-local generated classes in the callback by extracting
   * information about its names and using the names to generate class file paths.
   *
   * Mimics the previous logic that was present in `Analyzer`, despite the fact
   * that now we construct the names that the compiler will give to every non-local
   * class independently of genbcode.
   *
   * Why do we do this? The motivation is that we want to run the incremental algorithm
   * independently of the compiler pipeline. This independence enables us to:
   *
   * 1. Offload the incremental compiler logic out of the primary pipeline and
   *    run the incremental phases concurrently.
   * 2. Know before the compilation is completed whether another compilation will or
   *    will not be required. This is important to make incremental compilation work
   *    with pipelining and enables further optimizations; for example, we can start
   *    subsequent incremental compilations before (!) the initial compilation is done.
   *    This can buy us ~30-40% faster incremental compiler iterations.
   *
   * This method only takes care of non-local classes because local classes have no
   * relevance in the correctness of the algorithm and can be registered after genbcode.
   * Local classes are only used to construct the relations of products and to produce
   * the list of generated files + stamps, but names referring to local classes **never**
   * show up in the name hashes of classes' APIs, hence never considered for name hashing.
   *
   * As local class files are owned by other classes that change whenever they change,
   * we could most likely live without adding their class files to the products relation
   * and registering their stamps. However, to be on the safe side, we will continue to
   * register the local products in `Analyzer`.
   *
   * @param allClassSymbols The class symbols found in all the compilation units.
   */
  def registerGeneratedClasses(classSymbols: Iterator[Symbol]): Unit = {
    // Guard against a local class in case it surreptitiously leaks here
    classSymbols.filter(!_.isLocalClass).foreach { symbol =>
      val sourceFile = symbol.sourceFile
      val sourceVF0 =
        if (sourceFile == null) symbol.enclosingTopLevelClass.sourceFile
        else sourceFile
      val sourceVF: Option[VirtualFile] = sourceVF0 match {
        case AbstractZincFile(vf) => Some(vf)
        // This could be scala.reflect.io.FileZipArchive$LeakyEntry
        case _ => None
      }

      def registerProductNames(names: FlattenedNames): Unit = {
        val pathToClassFile = s"${names.binaryName}.class"
        val classFile = {
          JarUtils.outputJar match {
            case Some(outputJar) =>
              new java.io.File(JarUtils.classNameInJar(outputJar, pathToClassFile))
            case None =>
              val outputDir = global.settings.outputDirs.outputDirFor(sourceFile).file
              new java.io.File(outputDir, pathToClassFile)
          }
        }
        val zincClassName = names.className
        val srcClassName = classNameAsString(symbol)
        sourceVF foreach { source =>
          callback.generatedNonLocalClass(
            source,
            classFile.toPath,
            zincClassName,
            srcClassName
          )
        }
      }

      val names = FlattenedNames(
        fullName(symbol, java.io.File.separatorChar, symbol.moduleSuffix, includePackageObjectClassNames = true),
        fullName(symbol, '.', symbol.moduleSuffix, includePackageObjectClassNames = false)
      )

      registerProductNames(names)

      // Register the names of top-level module symbols that emit two class files
      val isTopLevelUniqueModule =
        symbol.owner.isPackageClass && symbol.isModuleClass && symbol.companionClass == NoSymbol
      if (isTopLevelUniqueModule || symbol.isPackageObject) {
        val names = FlattenedNames(
          fullName(symbol, java.io.File.separatorChar, "", includePackageObjectClassNames = true),
          fullName(symbol, '.', "", includePackageObjectClassNames = false)
        )
        registerProductNames(names)
      }
    }
  }

  private final class TopLevelHandler(extractApi: ExtractAPI[global.type])
      extends TopLevelTraverser {
    def allNonLocalClasses: Set[ClassLike] = {
      extractApi.allExtractedNonLocalClasses
    }

    def mainClasses: Set[String] = extractApi.mainClasses

    def `class`(c: Symbol): Unit = {
      extractApi.extractAllClassesOf(c.owner, c)
    }
  }

  private abstract class TopLevelTraverser extends Traverser {
    def `class`(s: Symbol): Unit
    override def traverse(tree: Tree): Unit = {
      tree match {
        case (_: ClassDef | _: ModuleDef) if isTopLevel(tree.symbol) => `class`(tree.symbol)
        case _: PackageDef =>
          super.traverse(tree)
        case _ =>
      }
    }
    def isTopLevel(sym: Symbol): Boolean = {
      !ignoredSymbol(sym) &&
      sym.isStatic &&
      (!sym.hasFlag(Flags.JAVA) || global.callback.isPickleJava) &&
      !sym.isNestedClass
    }
  }

}
