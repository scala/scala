/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

import scala.collection.mutable.HashMap

/**
 * @author Philipp Haller
 */
@serializable
class Proxy(node: Node, name: Symbol, @transient var kernel: NetKernel) extends AbstractActor {
  import java.io.{IOException, ObjectOutputStream, ObjectInputStream}

  @transient
  private[remote] var del: Actor = null
  startDelegate()

  @throws(classOf[IOException])
  private def writeObject(out: ObjectOutputStream) {
    out.defaultWriteObject()
  }

  @throws(classOf[ClassNotFoundException]) @throws(classOf[IOException])
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    setupKernel()
    startDelegate()
  }

  private def startDelegate() {
    del = new DelegateActor(this, node, name, kernel)
    del.start()
  }

  private def setupKernel() {
    kernel = RemoteActor.someKernel
    kernel.registerProxy(node, name, this)
  }

  def !(msg: Any): Unit =
    del ! msg

  def send(msg: Any, replyCh: OutputChannel[Any]): Unit =
    del.send(msg, replyCh)

  def forward(msg: Any): Unit =
    del.forward(msg)

  def receiver: Actor =
    del

  def !?(msg: Any): Any =
    del !? msg

  def !?(msec: Long, msg: Any): Option[Any] =
    del !? (msec, msg)

  def !!(msg: Any): Future[Any] =
    del !! msg

  def !![A](msg: Any, f: PartialFunction[Any, A]): Future[A] =
    del !! (msg, f)

  def linkTo(to: AbstractActor): Unit =
    del ! Apply0(new LinkToFun)

  def unlinkFrom(from: AbstractActor): Unit =
    del ! Apply0(new UnlinkFromFun)

  def exit(from: AbstractActor, reason: AnyRef): Unit =
    del ! Apply0(new ExitFun(reason))

  override def toString() =
    name+"@"+node
}

@serializable
private[remote] class LinkToFun extends Function2[AbstractActor, Proxy, Unit] {
  def apply(target: AbstractActor, creator: Proxy) {
    target.linkTo(creator)
  }
  override def toString =
    "<LinkToFun>"
}

@serializable
private[remote] class UnlinkFromFun extends Function2[AbstractActor, Proxy, Unit] {
  def apply(target: AbstractActor, creator: Proxy) {
    target.unlinkFrom(creator)
  }
  override def toString =
    "<UnlinkFromFun>"
}

@serializable
private[remote] class ExitFun(reason: AnyRef) extends Function2[AbstractActor, Proxy, Unit] {
  def apply(target: AbstractActor, creator: Proxy) {
    target.exit(creator, reason)
  }
  override def toString =
    "<ExitFun>("+reason.toString+")"
}

private[remote] case class Apply0(rfun: Function2[AbstractActor, Proxy, Unit])

/**
 * @author Philipp Haller
 */
private[remote] class DelegateActor(creator: Proxy, node: Node, name: Symbol, kernel: NetKernel) extends Actor {
  var channelMap = new HashMap[Symbol, OutputChannel[Any]]
  var sessionMap = new HashMap[Channel[Any], Symbol]

  def act() {
    Actor.loop {
      react {
        case cmd@Apply0(rfun) =>
          kernel.remoteApply(node, name, sender, rfun)

        case cmd@LocalApply0(rfun, target) =>
          rfun(target, creator)

        // Request from remote proxy.
        // `this` is local proxy.
        case cmd@SendTo(out, msg, session) =>
          // is this an active session?
          channelMap.get(session) match {
            case None =>
              // create a new reply channel...
              val replyCh = new Channel[Any](this)

              // ...that maps to session
              sessionMap += Pair(replyCh, session)

              // local send
              out.send(msg, replyCh)

            case Some(replyCh) =>
              replyCh ! msg
              // TODO:
              // remove `replyCh` from mapping
              // to avoid memory leak (always safe?)
              // or: use WeakHashMap
              // however, it's the value (channel)
              // that should be weak!
          }

        case cmd@Terminate =>
          exit()

        // local proxy receives response to
        // reply channel
        case ch ! resp =>
          // lookup session ID
          sessionMap.get(ch) match {
            case Some(sid) =>
              val msg = resp.asInstanceOf[AnyRef]
              // send back response
              kernel.forward(sender, node, name, msg, sid)

            case None =>
              Debug.info(this+": cannot find session for "+ch)
          }

        // remote proxy receives request
        case msg: AnyRef =>
          // create fresh session ID...
          val sid = FreshNameCreator.newName(node+"@"+name)

          // ...that maps to reply channel
          channelMap += Pair(sid, sender)

          kernel.forward(sender, node, name, msg, sid)
      }
    }
  }

}
