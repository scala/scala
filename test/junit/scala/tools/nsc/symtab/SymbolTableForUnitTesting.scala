package scala.tools.nsc
package symtab

import scala.reflect.internal.{Phase, NoPhase, SomePhase}
import scala.tools.util.PathResolver
import util.ClassPath
import io.AbstractFile

/**
 * A complete SymbolTable implementation designed to be used in JUnit tests.
 *
 * It enables `usejavacp` setting so classpath of JUnit runner is being used
 * for symbol table's classpath.
 *
 * This class contains enough of logic implemented to make it possible to
 * initialize definitions and inspect symbols.
 */
class SymbolTableForUnitTesting extends SymbolTable {
  // Members declared in scala.reflect.api.Trees
  override def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  override def newLazyTreeCopier: TreeCopier = new LazyTreeCopier
  trait TreeCopier extends InternalTreeCopierOps
  // these should be mocks
  class StrictTreeCopier extends super.StrictTreeCopier with TreeCopier
  class LazyTreeCopier extends super.LazyTreeCopier with TreeCopier

  override def isCompilerUniverse: Boolean = true
  def classPath = new PathResolver(settings).result

  object platform extends backend.Platform {
    val symbolTable: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this
    lazy val loaders: SymbolTableForUnitTesting.this.loaders.type = SymbolTableForUnitTesting.this.loaders
    def platformPhases: List[SubComponent] = Nil
    val classPath: ClassPath[AbstractFile] = new PathResolver(settings).result
    def doLoad(cls: ClassPath[AbstractFile]#ClassRep): Boolean = true
    def isMaybeBoxed(sym: Symbol): Boolean = ???
    def needCompile(bin: AbstractFile, src: AbstractFile): Boolean = ???
    def externalEquals: Symbol = ???
    def updateClassPath(subst: Map[ClassPath[AbstractFile], ClassPath[AbstractFile]]): Unit = ???
  }

  object loaders extends symtab.SymbolLoaders {
    val symbolTable: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this
    lazy val platform: symbolTable.platform.type = symbolTable.platform
    def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol =
      sym.info.member(name)
    protected override def compileLate(srcfile: AbstractFile): Unit =
      sys.error(s"We do not expect compileLate to be called in SymbolTableTest. The srcfile passed in is $srcfile")
  }

  class GlobalMirror extends Roots(NoSymbol) {
    val universe: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this
    def rootLoader: LazyType = new loaders.PackageLoader(classPath)
    override def toString = "compiler mirror"
  }

  lazy val rootMirror: Mirror = {
    val rm = new GlobalMirror
    rm.init()
    rm.asInstanceOf[Mirror]
  }

  def settings: Settings = {
    val s = new Settings
    // initialize classpath using java classpath
    s.usejavacp.value = true
    s
  }

   // Members declared in scala.reflect.internal.Required
  def picklerPhase: scala.reflect.internal.Phase = SomePhase

  // Members declared in scala.reflect.internal.SymbolTable
  def currentRunId: Int = 1
  def log(msg: => AnyRef): Unit = println(msg)
  def mirrorThatLoaded(sym: Symbol): Mirror = rootMirror
  val phases: Seq[Phase] = List(NoPhase, SomePhase)
  val phaseWithId: Array[Phase] = {
    val maxId = phases.map(_.id).max
    val phasesArray = Array.ofDim[Phase](maxId+1)
    phases foreach { phase =>
      phasesArray(phase.id) = phase
    }
    phasesArray
  }
  lazy val treeInfo: scala.reflect.internal.TreeInfo{val global: SymbolTableForUnitTesting.this.type} = ???

  phase = SomePhase
}
