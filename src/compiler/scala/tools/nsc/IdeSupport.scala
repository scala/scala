/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import scala.tools.nsc.io._

trait IdeSupport extends Global with symtab.IdeSupport {
  /** to do no dependency tracking */
  protected def normalCompile[T](f: => T): T = f
  override def unpickleIDEHook: (( => Type) => Type) = f => normalCompile(f)

  class IdeRun extends Run {
    override def compiles(sym: Symbol): Boolean = false // throw new Error
    override def compileLate(file: AbstractFile) = {
      reloadSource(file)
      normalCompile(super.compileLate(file))
    }
    override def stopPhase(name : String) =
      name == "superaccessors" || super.stopPhase(name)
  }

  // load a source file without us caring about adapt.
  def loadSource(file: AbstractFile): Option[CompilationUnit] = {
    val run = new IdeRun
    reloadSource(file)
    val source = getSourceFile(file)
    try {
      normalCompile(run.compileSources(source :: Nil))
      run.units.find(_.source == source)
    } catch {
      case e =>
        logError("error in presentation normal compile ", e)
        None
    }
  }

  object loaders1 extends {
    val global: IdeSupport.this.type = IdeSupport.this
  } with scala.tools.nsc.symtab.SymbolLoaders {
    import global._
    protected override def completeClassfile(root: global.Symbol, loader: ClassfileLoader)(f: => Unit) {
      global.normalCompile(f)
    }
    override def computeDepends(from: PackageLoader): global.PackageScopeDependMap =
      IdeSupport.this.computeDepends(from.asInstanceOf[IdeSupport.this.loaders.PackageLoader])
  }

  def computeDepends(from: loaders.PackageLoader): PackageScopeDependMap = null
  override lazy val loaders = loaders1
}
