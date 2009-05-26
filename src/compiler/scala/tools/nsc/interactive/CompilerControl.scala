package scala.tools.nsc.interactive

import scala.concurrent.SyncVar
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, WorkScheduler}
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** Interface of interactive compiler to a client such as an IDE
 */
trait CompilerControl { self: Global =>

  /* Must be initialized before starting compilerRunner */
  protected val scheduler = new WorkScheduler

  /** The compilation unit corresponding to a source file */
  def unitOf(s: SourceFile): RichCompilationUnit = unitOfFile get s.file match {
    case Some(unit) =>
      unit
    case None =>
      val unit = new RichCompilationUnit(s)
      unitOfFile(s.file) = unit
      unit
  }

  /** The compilation unit corresponding to a position */
  def unitOf(pos: Position): RichCompilationUnit = unitOf(pos.source.get)

  /** Locate smallest tree that encloses position */
  def locateTree(pos: Position): Tree =
    new Locator(pos) locateIn unitOf(pos).body

  /** Locate smallest context that encloses position */
  def locateContext(pos: Position): Option[Context] =
    locateContext(unitOf(pos).contexts, pos)

  /** Make sure a set of compilation units is loaded and parsed */
  def askReload(sources: List[SourceFile], result: SyncVar[Either[Unit, Throwable]]) =
    scheduler.postWorkItem(() => reload(sources, result))

  /** Set sync var `result` to a fully attributed tree located at position `pos`  */
  def askTypeAt(pos: Position, result: SyncVar[Either[Tree, Throwable]]) =
    scheduler.postWorkItem(() => self.typedTreeAt(pos, result))

  /** Ask to do unit first on present and subsequent type checking passes */
  def askToDoFirst(f: SourceFile) = {
    scheduler.postWorkItem { () => moveToFront(List(f)) }
  }

  /** Cancel currently pending high-priority jobs */
  def askCancel() =
    scheduler.raise(new CancelActionReq)

  /** Cancel current compiler run and start a fresh one where everything will be re-typechecked
   *  (but not re-loaded).
   */
  def askReset() =
    scheduler.raise(new FreshRunReq)

  /** Tell the compile server to shutdown, and do not restart again */
  def askShutdown() =
    scheduler.raise(new ShutdownReq)

  // ---------------- Interpreted exeptions -------------------

  class CancelActionReq extends Exception
  class FreshRunReq extends Exception
  class ShutdownReq extends Exception

  // ---------------- Helper class -----------------------------

  /** A locator for trees with given positions.
   *  Given a position `pos`, locator.apply returns
   *  the smallest tree that encloses `pos`.
   */
  private class Locator(pos: Position) extends Traverser {
    var last: Tree = _
    def locateIn(root: Tree): Tree = {
      this.last = EmptyTree
      traverse(root)
      this.last
    }
    override def traverse(t: Tree) {
      if (!t.pos.isSynthetic && (t.pos includes pos)) {
        last = t
        super.traverse(t)
      }
    }
  }
}
