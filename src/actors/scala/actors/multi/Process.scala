/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.multi

import scala.collection.mutable.{HashMap,HashSet,Stack}

/**
 * @author Philipp Haller
 */
object Process {

  def spawn(body: => Unit): Process = {
    val p = new Process {
      override def run(): Unit = body
    }
    val task = new ReceiverTask(p, null) {
      override def run(): Unit = try { p.start() }
      catch {
        case d: Done =>
          // do nothing (continuation is already saved)
      }
    }
    Scheduler.execute(task)
    p
  }

  def spawnLink(body: => Unit): Process = {
    val p = new Process {
      override def run(): Unit = body
    }
    val task = new ReceiverTask(p, null) {
      override def run(): Unit = try { p.start() }
      catch {
        case d: Done =>
          // do nothing (continuation is already saved)
      }
    }
    self.link(p)
    Scheduler.execute(task)
    p
  }

  def send(p: Process, msg: Any) =
    p ! msg

  def receive(f: PartialFunction[Any,Unit]): Nothing =
    self.receive(f)

  def receiveWithin(msec: long)(f: PartialFunction[Any,Unit]): Nothing =
    self.receiveWithin(msec)(f)

  def self: Process = {
    val p = Scheduler.getProcess(Thread.currentThread())
    if (p == null)
      error("Self called outside a process")
    else
      p
  }

  def exit(p: Process, reason: Symbol) =
    p.exit(reason)
}

/**
 * @author Philipp Haller
 */
class Process extends scala.actors.Process with Actor[Any] {
  private val links = new HashSet[scala.actors.Process]

  override def start(): Unit = try { run() }
  catch {
    case d: Done => // do nothing
    case t: Throwable => {
      if (!excHandlerDescs.isEmpty)
        forwardExc(excHandlerDescs.top, t)
      else
        exit(new Symbol(t.toString()))
    }
  }

  case class Exit(from: scala.actors.Process, reason: Symbol)

  def link(to: scala.actors.Process): Unit = {
    links += to
    to.linkTo(this)
  }

  def linkTo(to: scala.actors.Process): Unit =
    links += to

  def unlink(from: scala.actors.Process): Unit = {
    links -= from
    from.unlinkFrom(this)
  }

  def unlinkFrom(from: scala.actors.Process): Unit =
    links -= from

  private var trapExit = false

  def processFlag(flag: Symbol, set: boolean) = {
    if (flag.name.equals("trapExit")) trapExit = set
  }

  def exit(reason: Symbol): Unit = {
    exitLinked(reason, new HashSet[Process])
    if (isAlive) isAlive = false
  }

  def exit(from: scala.actors.Process, reason: Symbol): Unit = {
    if (from == this) {
      exit(reason)
    }
    else {
      if (trapExit)
        this ! Exit(from, reason)
      else if (!reason.name.equals("normal"))
        exit(reason)
    }
  }

  def exitLinked(reason: Symbol, exitMarks: HashSet[Process]): Unit = {
    if (exitMarks contains this) {
      // we are marked, do nothing
    }
    else {
      exitMarks += this // mark this as exiting

      // exit linked processes
      val iter = links.elements
      while (iter.hasNext) {
        val linked = iter.next
        unlink(linked)
        linked.exit(this, reason)
      }
      exitMarks -= this
    }
  }

  val excHandlerDescs = new Stack[ExcHandlerDesc]

  // exception handler table
  val excHandlers = new HashMap[Int, ExcHandler]

  def forwardExc(destDesc: ExcHandlerDesc, e: Throwable) = {
    // locality check (handler local to this actor?)
    if (destDesc.p == this) {
      handleExc(destDesc, e)
    }
    else {
      // forward to process of destination descriptor
      destDesc.p.handleExc(destDesc, e)
    }
  }

  def pushExcHandlerDesc(desc: ExcHandlerDesc) =
    excHandlerDescs += desc

  /** is only called for local handlers
      (i.e. destDesc.p == this) */
  def handleExc(destDesc: ExcHandlerDesc, e: Throwable) =
    (excHandlers get destDesc.eid) match {
      case Some(handler) =>
        handler.handle(e)
      case None =>
        error("exc desc refers to non-registered handler")
    }

  private var excCnt = 0
  private def freshExcId = { excCnt = excCnt + 1; excCnt }

  def tryAsync(block: => Unit,
               handlerFun: PartialFunction[Throwable, Unit]) = {
    val excHandler =
      new ExcHandler(handlerFun,
                     this,
                     if (excHandlerDescs.isEmpty) null
                     else excHandlerDescs.top)

    val desc = ExcHandlerDesc(this, freshExcId)
    // associate desc with handler
    excHandlers += desc.eid -> excHandler
    // push desc onto stack
    excHandlerDescs += desc
    //Console.println("desc stack height: " + excHandlerDescs.length)
    // execute code block
    block
  }

  def process(f: PartialFunction[Any,unit], msg: Any): unit = {
    try {
      f(msg)
    }
    catch {
      case d: Done =>
        throw new Done
      case t: Throwable =>
        throw t
        if (!excHandlerDescs.isEmpty)
          forwardExc(excHandlerDescs.top, t)
        else
          die(Symbol(t.toString()))
    }
  }

  override def receiveMsg(msg: Any) = {
    //Debug.info("" + Thread.currentThread() + ": Resuming " + this)
    if (continuation != null) {
      val f = continuation
      continuation = null
      scheduled = false
      process(f, msg)
      die()
    }
    else {
      // use more complex receive-and-return continuation
      val cases = contCases
      val then = contThen
      contCases = null
      contThen = null
      scheduled = false
      val result = cases(msg)
      then(result)
      die()
    }
  }

  def die(reason: Symbol) = {
    if (isAlive) {
      isAlive = false
      //Debug.info("" + this + " died.")
      exit(reason)
    }
  }

  override def die() = {
    if (isAlive) {
      isAlive = false
      //Debug.info("" + this + " died.")
      exit('normal)
    }
  }
}

case class ExcHandlerDesc(p: Process, eid: Int)

class ExcHandler(actions: PartialFunction[Throwable, Unit],
                 p: Process,
                 parent: ExcHandlerDesc) {
  def handle(e: Throwable): Unit = {
    if (!actions.isDefinedAt(e)) {
      if (parent != null) p.forwardExc(parent, e)
    }
    else
      actions(e)
  }
}
