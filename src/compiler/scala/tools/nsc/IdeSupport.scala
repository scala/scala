package scala.tools.nsc
import scala.tools.nsc.io._
trait IdeSupport extends Global with symtab.IdeSupport {
  /** to do no dependency tracking */
  protected def normalCompile[T](f : => T) : T = f
  override def unpickleIDEHook : (( => Type) => Type) = f => normalCompile(f)
  class IdeRun extends Run {
    override def compiles(sym : Symbol) : Boolean = throw new Error
    override def compileLate(file : AbstractFile) = {
      // don't bother with any of the phase crap since adapt isn't supported
      reloadSource(file)
      normalCompile(super.compileLate(file))
    }
    override def stopPhase(name : String) =
      name == "superaccessors" || super.stopPhase(name)
  }


  // load a source file without us caring about adapt.
  def loadSource(file : AbstractFile) = {
    val run = new IdeRun
    reloadSource(file)
    val source = getSourceFile(file)
    normalCompile(run.compileSources(source :: Nil))
    run.units.find(unit => unit.source == source)
  }
  object loaders1 extends scala.tools.nsc.symtab.SymbolLoaders {
    lazy val global : IdeSupport.this.type = IdeSupport.this
    import global._
    protected override def completeClassfile(root : global.Symbol, loader : ClassfileLoader)(f : => Unit) : Unit =
      global.normalCompile(f)
  }
  override lazy val loaders = loaders1
}
