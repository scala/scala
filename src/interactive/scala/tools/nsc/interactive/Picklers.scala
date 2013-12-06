/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

import util.InterruptReq
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }
import io.{ AbstractFile, PlainFile }
import util.EmptyAction
import scala.reflect.internal.util.Position
import Pickler._
import scala.collection.mutable
import mutable.ListBuffer

trait Picklers { self: Global =>

  lazy val freshRunReq =
    unitPickler
      .wrapped { _ => new FreshRunReq } { x => () }
      .labelled ("FreshRunReq")
      .cond (_.isInstanceOf[FreshRunReq])

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

  lazy val offsetPosition: CondPickler[Position] =
    (pkl[SourceFile] ~ pkl[Int])
      .wrapped { case x ~ y => Position.offset(x, y) } { p => p.source ~ p.point }
      .asClass (classOf[Position])

  lazy val rangePosition: CondPickler[Position] =
    (pkl[SourceFile] ~ pkl[Int] ~ pkl[Int] ~ pkl[Int])
      .wrapped { case source ~ start ~ point ~ end => Position.range(source, start, point, end) } { p => p.source ~ p.start ~ p.point ~ p.end }
      .asClass (classOf[Position])

  lazy val transparentPosition: CondPickler[Position] =
    (pkl[SourceFile] ~ pkl[Int] ~ pkl[Int] ~ pkl[Int])
      .wrapped { case source ~ start ~ point ~ end => Position.range(source, start, point, end).makeTransparent } { p => p.source ~ p.start ~ p.point ~ p.end }
      .asClass (classOf[Position])

  lazy val noPosition = singletonPickler(NoPosition)

  implicit lazy val position: Pickler[Position] = transparentPosition | rangePosition | offsetPosition | noPosition

  implicit lazy val namePickler: Pickler[Name] =
    pkl[String] .wrapped[Name] {
      str => if ((str.length > 1) && (str endsWith "!")) newTypeName(str.init) else newTermName(str)
    } {
      name => if (name.isTypeName) name.toString+"!" else name.toString
    }

  implicit lazy val symPickler: Pickler[Symbol] = {
    def ownerNames(sym: Symbol, buf: ListBuffer[Name]): ListBuffer[Name] = {
      if (!sym.isRoot) {
        ownerNames(sym.owner, buf)
        buf += (if (sym.isModuleClass) sym.sourceModule else sym).name
        if (!sym.isType && !sym.isStable) { // TODO: what's the reasoning behind this condition!?
          val sym1 = sym.owner.info.decl(sym.name)
          if (sym1.isOverloaded) {
            val index = sym1.alternatives.indexOf(sym)
            assert(index >= 0, sym1+" not found in alternatives "+sym1.alternatives)
            buf += newTermName(index.toString)
          }
        }
      }
      buf
    }
    def makeSymbol(root: Symbol, names: List[Name]): Symbol = names match {
      case List() =>
        root
      case name :: rest =>
        val sym = root.info.decl(name)
        if (sym.isOverloaded) makeSymbol(sym.alternatives(rest.head.toString.toInt), rest.tail)
        else makeSymbol(sym, rest)
    }
    pkl[List[Name]] .wrapped { makeSymbol(rootMirror.RootClass, _) } { ownerNames(_, new ListBuffer).toList }
  }

  implicit def workEvent: Pickler[WorkEvent] = {
    (pkl[Int] ~ pkl[Long])
      .wrapped { case id ~ ms => WorkEvent(id, ms) } { w => w.atNode ~ w.atMillis }
  }

  implicit def interruptReq: Pickler[InterruptReq] = {
    val emptyIR: InterruptReq = new InterruptReq { type R = Unit; val todo = () => () }
    pkl[Unit] .wrapped { _ =>  emptyIR } { _ => () }
  }

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

  implicit def askLinkPosItem: CondPickler[AskLinkPosItem] =
    (pkl[Symbol] ~ pkl[SourceFile])
      .wrapped { case sym ~ source => new AskLinkPosItem(sym, source, new Response) } { item => item.sym ~ item.source }
      .asClass (classOf[AskLinkPosItem])

  implicit def askDocCommentItem: CondPickler[AskDocCommentItem] =
    (pkl[Symbol] ~ pkl[SourceFile] ~ pkl[Symbol] ~ pkl[List[(Symbol,SourceFile)]])
      .wrapped { case sym ~ source ~ site ~ fragments => new AskDocCommentItem(sym, source, site, fragments, new Response) } { item => item.sym ~ item.source ~ item.site ~ item.fragments }
      .asClass (classOf[AskDocCommentItem])

  implicit def askLoadedTypedItem: CondPickler[AskLoadedTypedItem] =
    pkl[SourceFile]
      .wrapped { source => new AskLoadedTypedItem(source, false, new Response) } { _.source }
      .asClass (classOf[AskLoadedTypedItem])

  implicit def askParsedEnteredItem: CondPickler[AskParsedEnteredItem] =
    (pkl[SourceFile] ~ pkl[Boolean])
      .wrapped { case source ~ keepLoaded => new AskParsedEnteredItem(source, keepLoaded, new Response) } { w => w.source ~ w.keepLoaded }
      .asClass (classOf[AskParsedEnteredItem])

  implicit def emptyAction: CondPickler[EmptyAction] =
    pkl[Unit]
      .wrapped { _ => new EmptyAction } { _ => () }
      .asClass (classOf[EmptyAction])

  implicit def action: Pickler[() => Unit] =
    reloadItem | askTypeAtItem | askTypeItem | askTypeCompletionItem | askScopeCompletionItem |
    askToDoFirstItem | askLinkPosItem | askDocCommentItem | askLoadedTypedItem | askParsedEnteredItem | emptyAction
}
