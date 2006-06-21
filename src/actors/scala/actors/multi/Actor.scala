package scala.actors.multi

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack

case class ExcHandlerDesc(pid: Pid, eid: int)

class ExcHandler(actions: PartialFunction[Throwable, unit],
                 actor: Actor,
                 parent: ExcHandlerDesc) {
  def handle(e: Throwable): unit = {
    if (!actions.isDefinedAt(e)) {
      if (parent != null) actor.forwardExc(parent, e)
    }
    else
      actions(e)
  }
}

/**
 * @author Philipp Haller
 */
abstract class Actor extends MailBox {
  def run(): Unit = {}

  def start(): Unit = {
    var finished = true
    try { run }
    catch {
      case d: Done =>
        finished = false
      case t: Throwable =>
        if (!excHandlerDescs.isEmpty)
          forwardExc(excHandlerDescs.top, t)
        else
          exit(new Symbol(t.toString()))
    }
    if (finished) die()
  }

  case class Exit(from: Pid, reason: Symbol) extends Message

  private var pid: Pid = null
  def self: Pid = {
    if (pid == null) pid = new LocalPid(this)
    pid
  }
  def self_= (p: Pid) = pid = p


  private val links = new HashSet[Pid]

  def link(to: Pid): unit = {
    // TODO: check if exists (eff: dont need to.linkTo...)
    links += to
    to.linkTo(self)
  }
  def linkTo(to: Pid): unit = links += to

  def unlink(from: Pid): unit = {
    // TODO: check if exists (eff)
    links -= from
    from.unlinkFrom(self)
  }
  def unlinkFrom(from: Pid): unit = links -= from


  private var trapExit = false

  def processFlag(flag: Symbol, set: boolean) = {
    if (flag.name.equals("trapExit")) trapExit = set
  }

  def exit(reason: Symbol): unit = {
    exitLinked(reason, new HashSet[Pid])
    if (isAlive) {
      isAlive = false
      //Debug.info("" + this + " died.")
    }
  }

  def exit(from: Pid, reason: Symbol): unit = {
    if (from == self) {
      exit(reason)
    }
    else {
      if (trapExit)
        this send Exit(from, reason)
      else if (!reason.name.equals("normal"))
        exit(reason)
    }
  }

  def exitLinked(reason: Symbol, exitMarks: HashSet[Pid]): unit = {
    if (exitMarks contains self) {
      // we are marked, do nothing
    }
    else {
      exitMarks += self // mark self as exiting
      //Console.println("" + self + " is exiting (" + reason + ").")

      // exit linked scala.actors
      val iter = links.elements
      while (iter.hasNext) {
        val linkedPid = iter.next
        unlink(linkedPid)
        linkedPid.exit(self, reason)
      }
      exitMarks -= self
    }
  }

  def spawnLink(body: Actor => unit): Pid = {
    val a = new Actor {
      override def run = body(this)
    }
    if (!excHandlerDescs.isEmpty)
      a.pushExcHandlerDesc(excHandlerDescs.top)
    // link new process to self
    link(a.self)
    a.start
    a.self
  }

  val excHandlerDescs = new Stack[ExcHandlerDesc]

  // exception handler table
  val excHandlers = new HashMap[int, ExcHandler]

  def forwardExc(destDesc: ExcHandlerDesc, e: Throwable) = {
    // locality check (handler local to this actor?)
    if (destDesc.pid == self) {
      handleExc(destDesc, e)
    }
    else {
      // forward to pid of destination descriptor
      destDesc.pid.handleExc(destDesc, e)
    }
  }

  def pushExcHandlerDesc(desc: ExcHandlerDesc) = {
    excHandlerDescs += desc
  }

  /** is only called for local handlers
      (i.e. destDesc.pid == self) */
  def handleExc(destDesc: ExcHandlerDesc, e: Throwable) =
    (excHandlers get destDesc.eid) match {
      case Some(handler) =>
        handler.handle(e)
      case None =>
        error("exc desc refers to non-registered handler")
    };

  var excCnt = 0
  def freshExcId = { excCnt = excCnt + 1; excCnt }

  def tryAsync(block: => unit,
               handlerFun: PartialFunction[Throwable, unit]) = {
    val excHandler =
      new ExcHandler(handlerFun,
                     this,
                     if (excHandlerDescs.isEmpty) null
                     else excHandlerDescs.top)

    val desc = ExcHandlerDesc(self, freshExcId)
    // associate desc with handler
    excHandlers += desc.eid -> excHandler
    // push desc onto stack
    excHandlerDescs += desc
    //Console.println("desc stack height: " + excHandlerDescs.length)
    // execute code block
    block
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

  def process(f: PartialFunction[Message,unit], msg: Message): unit = {
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

  override def receive(f: PartialFunction[Message,unit]): scala.All = {
    if (isAlive) {
      Scheduler.tick(this)
      continuation = null
      sent.dequeueFirst(f.isDefinedAt) match {
        case Some(msg) =>
          process(f, msg)
          die()
        case None =>
          continuation = f
          //Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
      }
    }
    throw new Done
  }

  override def receiveMsg(msg: MailBox#Message) = {
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

  def spawn(a: Actor): Pid = {
    // let "a" inherit active exception handler
    if (!excHandlerDescs.isEmpty)
      a.pushExcHandlerDesc(excHandlerDescs.top)
    a.start
    a.self
  }

  def spawn(body: Actor => unit): Pid = {
    val a = new Actor {
      override def run = body(this)
    }
    if (!excHandlerDescs.isEmpty)
      a.pushExcHandlerDesc(excHandlerDescs.top)
    a.start
    a.self
  }

  def spawnReceive(cases: PartialFunction[MailBox#Message,unit]) = {
    val a = new Actor {
      override def run = receive(cases)
    }
    if (!excHandlerDescs.isEmpty)
      a.pushExcHandlerDesc(excHandlerDescs.top)
    a.start
    a.self
  }

  def join(pid1: Pid, pid2: Pid, cont: List[Pid]): unit = {
    receive {
      case Pair(pid1, msg1) => receive {
        case Pair(pid2, msg2) => cont match {
          case x::xs => x ! Pair(self, Pair(Pair(msg1, msg2), xs))
        }
      }
      case Pair(pid2, msg2) => receive {
        case Pair(pid1, msg1) => cont match {
          case x::xs => x ! Pair(self, Pair(Pair(msg1, msg2), xs))
        }
      }
    }
  }

  def makeRef = Actor.makeRef
}

object Actor {
  private var counter = 0
  type Tag = int
  def makeRef: Tag = {
    counter = counter + 1
    counter
  }
}
