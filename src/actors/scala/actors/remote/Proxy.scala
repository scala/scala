/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

import scala.collection.mutable.HashMap

/**
 * @version 0.9.17
 * @author Philipp Haller
 */
@serializable
class Proxy(node: Node, name: Symbol, @transient var kernel: NetKernel) extends OutputChannel[Any] {
  import java.io.{IOException, ObjectOutputStream, ObjectInputStream}

  @transient
  private[remote] var del: Actor = null
  startDelegate()

  @throws(classOf[IOException])
  private def writeObject(out: ObjectOutputStream) {
    Debug.info("Serializing "+this)
    out.defaultWriteObject()
  }

  @throws(classOf[ClassNotFoundException]) @throws(classOf[IOException])
  private def readObject(in: ObjectInputStream) {
    Debug.info("Deserializing "+this)
    in.defaultReadObject()
    setupKernel()
    startDelegate()
  }

  private def startDelegate() {
    del = new DelegateActor(node, name, kernel)
    del.start()
  }

  private def setupKernel() {
    kernel = RemoteActor.someKernel
    kernel.registerProxy(node, name, this)
  }

  def !(msg: Any): Unit =
    del.send(msg, Actor.self)

  def send(msg: Any, replyCh: OutputChannel[Any]): Unit =
    del.send(msg, replyCh)

  def forward(msg: Any): Unit =
    del.send(msg, Actor.sender)

  def receiver: Actor =
    del

  def !!(msg: Any): Future[Any] =
    del !! msg

  override def toString() =
    name+"@"+node
}

/**
 * @version 0.9.17
 * @author Philipp Haller
 */
private[remote] class DelegateActor(node: Node, name: Symbol, kernel: NetKernel) extends Actor {
  var channelMap = new HashMap[Symbol, OutputChannel[Any]]
  var sessionMap = new HashMap[Channel[Any], Symbol]

  def act() {
    Debug.info(this+": waiting to process commands")
    Actor.loop {
      react {
        // Request from remote proxy.
        // `this` is local proxy.
        case cmd@SendTo(out, msg, session) =>
          Debug.info(this+": processing "+cmd)

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
          Debug.info(this+": processing "+cmd)
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
