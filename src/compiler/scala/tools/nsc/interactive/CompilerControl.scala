package scala.tools.nsc
package interactive

import scala.concurrent.SyncVar
import scala.util.control.ControlException
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, WorkScheduler}
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** Interface of interactive compiler to a client such as an IDE
 */
trait CompilerControl { self: Global =>

  /** Response wrapper to client
   */
  type Response[T] = SyncVar[Either[T, Throwable]]

  abstract class WorkItem extends (() => Unit)

  /** Info given for every member found by completion
   */
  case class Member(val sym: Symbol, val tpe: Type, val accessible: Boolean, val inherited: Boolean, val viaView: Symbol) {
    def shadows(other: Member) =
      sym.name == other.sym.name && (sym.tpe matches other.sym.tpe)
  }

  /** The scheduler by which client and compiler communicate
   *  Must be initialized before starting compilerRunner
   */
  protected val scheduler = new WorkScheduler

  /** The compilation unit corresponding to a source file
   */
  def unitOf(s: SourceFile): RichCompilationUnit = unitOfFile get s.file match {
    case Some(unit) =>
      unit
    case None =>
      val unit = new RichCompilationUnit(s)
      unitOfFile(s.file) = unit
      unit
  }

  /** The compilation unit corresponding to a position */
  def unitOf(pos: Position): RichCompilationUnit = unitOf(pos.source)

  /** Remove the CompilationUnit corresponding to the given SourceFile
   *  from consideration for recompilation.
   */
  def removeUnitOf(s: SourceFile) = unitOfFile remove s.file

  /** Locate smallest tree that encloses position
   */
  def locateTree(pos: Position): Tree =
    new Locator(pos) locateIn unitOf(pos).body

  /** Locate smallest context that encloses position
   */
  def locateContext(pos: Position): Option[Context] =
    locateContext(unitOf(pos).contexts, pos)

  /** Make sure a set of compilation units is loaded and parsed.
   *  Return () to syncvar `result` on completion.
   */
  def askReload(sources: List[SourceFile], result: Response[Unit]) =
    scheduler postWorkItem new WorkItem {
      def apply() = reload(sources, result)
      override def toString = "reload "+sources
    }

  /** Set sync var `result` to a fully attributed tree located at position `pos`
   */
  def askTypeAt(pos: Position, result: Response[Tree]) =
    scheduler postWorkItem new WorkItem {
      def apply() = self.getTypedTreeAt(pos, result)
      override def toString = "typeat "+pos.source+" "+pos.show
    }

  def askCompletion(pos: Position, result: Response[List[Member]]) =
    scheduler postWorkItem new WorkItem {
      def apply() = self.completion(pos, result)
      override def toString = "completion "+pos.source+" "+pos.show
    }

  /** Ask to do unit first on present and subsequent type checking passes */
  def askToDoFirst(f: SourceFile) = {
    scheduler postWorkItem new WorkItem {
      def apply() = moveToFront(List(f))
      override def toString = "dofirst "+f
    }
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

  class CancelActionReq extends Exception with ControlException
  class FreshRunReq extends Exception with ControlException
  class ShutdownReq extends Exception with ControlException

}
