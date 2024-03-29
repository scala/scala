package scala.tools.nsc
package symtab

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scala.reflect.internal.{NoPhase, Phase, Reporter, SomePhase}
import scala.reflect.internal.util.{CodeAction, Statistics}
import scala.tools.util.PathResolver
import util.ClassPath
import io.AbstractFile
import scala.tools.nsc.Reporting.WarningCategory

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
  override def newStrictTreeCopier: TreeCopier = new StrictTreeCopierForUnitTesting
  override def newLazyTreeCopier: TreeCopier = new LazyTreeCopierForUnitTesting
  trait TreeCopier extends InternalTreeCopierOps
  // these should be mocks
  class StrictTreeCopierForUnitTesting extends super.StrictTreeCopier with TreeCopier
  class LazyTreeCopierForUnitTesting extends super.LazyTreeCopier with TreeCopier

  override def isCompilerUniverse: Boolean = true

  def classPath: ClassPath = platform.classPath

  object platform extends backend.Platform {
    val symbolTable: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this
    lazy val loaders: SymbolTableForUnitTesting.this.loaders.type = SymbolTableForUnitTesting.this.loaders

    def platformPhases: List[SubComponent] = Nil

    private[nsc] lazy val classPath: ClassPath = new PathResolver(settings, new CloseableRegistry).result

    def isMaybeBoxed(sym: Symbol): Boolean = ???
    def needCompile(bin: AbstractFile, src: AbstractFile): Boolean = ???
    def externalEquals: Symbol = ???
    def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit = ???
  }

  object loaders extends symtab.SymbolLoaders {
    val symbolTable: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this
    lazy val platform: symbolTable.platform.type = symbolTable.platform
    def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol =
      sym.info.member(name)
    protected override def compileLate(srcfile: AbstractFile): Unit =
      sys.error(s"We do not expect compileLate to be called in SymbolTableTest. The srcfile passed in is $srcfile")
    def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit =
      reporter.warning(pos, msg)
  }

  class GlobalMirror extends Roots(NoSymbol) {
    val universe: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this

    def rootLoader: LazyType = new loaders.PackageLoader(ClassPath.RootPackage, classPath)

    override def toString = "compiler mirror"
  }

  lazy val rootMirror: Mirror = {
    val rm = new GlobalMirror
    rm.init()
    rm.asInstanceOf[Mirror]
  }

  lazy val settings: Settings = {
    val s = new Settings
    // initialize classpath using java classpath
    s.usejavacp.value = true
    s
  }

  override lazy val statistics = new Statistics(this, settings) with ReflectStats

   // Members declared in scala.reflect.internal.Required
  def picklerPhase: scala.reflect.internal.Phase = SomePhase
  def erasurePhase: scala.reflect.internal.Phase = SomePhase

  // Members declared in scala.reflect.internal.Reporting
  def reporter = new Reporter {
    @nowarn("msg=overriding method info0")
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = println(msg)
  }

  // minimal Run to get Reporting wired
  def currentRun = new RunReporting {}
  class PerRunReporting extends PerRunReportingBase {
    def deprecationWarning(pos: Position, msg: String, since: String, site: String, origin: String, actions: List[CodeAction]): Unit = reporter.warning(pos, msg)
  }
  protected def PerRunReporting = new PerRunReporting

  // Members declared in scala.reflect.internal.SymbolTable
  def currentRunId: Int = 1
  def log(msg: => AnyRef): Unit = println(msg)
  def mirrorThatLoaded(sym: Symbol): Mirror = rootMirror
  val phases: Seq[Phase] = List(NoPhase, SomePhase)
  phases foreach { phase =>
    phaseWithId(phase.id) = phase
  }

  lazy val treeInfo = new scala.reflect.internal.TreeInfo {
    val global: SymbolTableForUnitTesting.this.type = SymbolTableForUnitTesting.this
  }

  val currentFreshNameCreator = new reflect.internal.util.FreshNameCreator

  phase = SomePhase

  type RuntimeClass = java.lang.Class[_]
  implicit val RuntimeClassTag: ClassTag[RuntimeClass] = ClassTag[RuntimeClass](classOf[RuntimeClass])
  implicit val MirrorTag: ClassTag[Mirror] = ClassTag[Mirror](classOf[GlobalMirror])
  implicit val TreeCopierTag: ClassTag[TreeCopier] = ClassTag[TreeCopier](classOf[TreeCopier])
}
