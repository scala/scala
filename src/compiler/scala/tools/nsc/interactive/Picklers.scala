package scala.tools.nsc
package interactive

import util.{SourceFile, BatchSourceFile}
import io.{AbstractFile, PlainFile}

import util.{Position, RangePosition, NoPosition, OffsetPosition, TransparentPosition, EmptyAction}
import io.{Pickler, CondPickler}
import io.Pickler._
import collection.mutable

trait Picklers { self: Global =>

  lazy val freshRunReq = singletonPickler(FreshRunReq)
  lazy val shutdownReq = singletonPickler(ShutdownReq)

  def defaultThrowable[T <: Throwable]: CondPickler[T] = javaInstancePickler[T] cond { _ => true }

  implicit lazy val throwable: Pickler[Throwable] =
    freshRunReq | shutdownReq | defaultThrowable

  implicit def abstractFile: Pickler[AbstractFile] =
    pkl[String]
      .wrapped[AbstractFile] { new PlainFile(_) } { _.path }
      .asClass (classOf[PlainFile])

  private val sourceFilesSeen = new mutable.HashMap[AbstractFile, Array[Char]] {
    override def default(key: AbstractFile) = Array()
  }

  type Diff = (Int /*start*/, Int /*end*/, String /*replacement*/)

  def delta(f: AbstractFile, cs: Array[Char]): Diff = {
    val bs = sourceFilesSeen(f)
    var start = 0
    while (start < bs.length && start < cs.length && bs(start) == cs(start)) start += 1
    var end = bs.length
    var end2 = cs.length
    while (end > start && end2 > start && bs(end - 1) == cs(end2 - 1)) { end -= 1; end2 -= 1 }
    sourceFilesSeen(f) = cs
    (start, end, cs.slice(start, end2).mkString(""))
  }

  def patch(f: AbstractFile, d: Diff): Array[Char] = {
    val (start, end, replacement) = d
    val patched = sourceFilesSeen(f).patch(start, replacement, end - start)
    sourceFilesSeen(f) = patched
    patched
  }

  implicit lazy val sourceFile: Pickler[SourceFile] =
    (pkl[AbstractFile] ~ pkl[Diff]).wrapped[SourceFile] {
      case f ~ d => new BatchSourceFile(f, patch(f, d))
    } {
      f => f.file ~ delta(f.file, f.content)
    }.asClass (classOf[BatchSourceFile])

  lazy val offsetPosition: CondPickler[OffsetPosition] =
    (pkl[SourceFile] ~ pkl[Int])
      .wrapped { case x ~ y => new OffsetPosition(x, y) } { p => p.source ~ p.point }
      .asClass (classOf[OffsetPosition])

  lazy val rangePosition: CondPickler[RangePosition] =
    (pkl[SourceFile] ~ pkl[Int] ~ pkl[Int] ~ pkl[Int])
      .wrapped { case source ~ start ~ point ~ end => new RangePosition(source, start, point, end) } { p => p.source ~ p.start ~ p.point ~ p.end }
      .asClass (classOf[RangePosition])

  lazy val transparentPosition: CondPickler[TransparentPosition] =
    (pkl[SourceFile] ~ pkl[Int] ~ pkl[Int] ~ pkl[Int])
      .wrapped { case source ~ start ~ point ~ end => new TransparentPosition(source, start, point, end) } { p => p.source ~ p.start ~ p.point ~ p.end }
      .asClass (classOf[TransparentPosition])

  lazy val noPosition = singletonPickler(NoPosition)

  implicit lazy val position: Pickler[Position] = transparentPosition | rangePosition | offsetPosition | noPosition

  implicit def reloadItem: CondPickler[ReloadItem] =
    pkl[List[SourceFile]]
      .wrapped { ReloadItem(_, new Response) } { _.sources }
      .asClass (classOf[ReloadItem])

  implicit def askTypeAtItem: CondPickler[AskTypeAtItem] =
    pkl[Position]
      .wrapped { new AskTypeAtItem(_, new Response) } { _.pos }
      .asClass (classOf[AskTypeAtItem])

  implicit def askTypeItem: CondPickler[AskTypeItem] =
    (pkl[SourceFile] ~ pkl[Boolean])
      .wrapped { case source ~ forceReload => new AskTypeItem(source, forceReload, new Response) } { w => w.source ~ w.forceReload }
      .asClass (classOf[AskTypeItem])

  implicit def askLastTypeItem: CondPickler[AskLastTypeItem] =
    pkl[SourceFile]
      .wrapped { new AskLastTypeItem(_, new Response) } { _.source }
      .asClass (classOf[AskLastTypeItem])

  implicit def askTypeCompletionItem: CondPickler[AskTypeCompletionItem] =
    pkl[Position]
      .wrapped { new AskTypeCompletionItem(_, new Response) } { _.pos }
      .asClass (classOf[AskTypeCompletionItem])

  implicit def askScopeCompletionItem: CondPickler[AskScopeCompletionItem] =
    pkl[Position]
      .wrapped { new AskScopeCompletionItem(_, new Response) } { _.pos }
      .asClass (classOf[AskScopeCompletionItem])

  implicit def askToDoFirstItem: CondPickler[AskToDoFirstItem] =
    pkl[SourceFile]
      .wrapped { new AskToDoFirstItem(_) } { _.source }
      .asClass (classOf[AskToDoFirstItem])

  /** We cannot pickle symbols, so we return always RootClass */
  implicit def askLinkPosItem: CondPickler[AskLinkPosItem] =
    pkl[SourceFile]
      .wrapped { new AskLinkPosItem(definitions.RootClass, _, new Response) } { _.source }
      .asClass (classOf[AskLinkPosItem])

  implicit def emptyAction: CondPickler[EmptyAction] =
    pkl[Unit]
      .wrapped { _ => new EmptyAction } { _ => () }
      .asClass (classOf[EmptyAction])

  implicit def action: Pickler[() => Unit] =
    reloadItem | askTypeAtItem | askTypeItem | askLastTypeItem | askTypeCompletionItem | askScopeCompletionItem | askToDoFirstItem | emptyAction
}
