/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import java.lang.{InterruptedException, Runnable}

/** <p>
 *    This exception is thrown whenever an actor exits.
 *    Its purpose is to let <code>exit</code> have
 *    return type <code>Nothing</code>.
 *  </p>
 *
 *  @version 0.9.10
 *  @author Philipp Haller
 */
private[actors] class ExitActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = this
}

private[actors] class KillActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = this
}

/** <p>
 *    The abstract class <code>Reaction</code> associates
 *    an instance of an <code>Actor</code> with a
 *    <a class="java/lang/Runnable" href="" target="contentFrame">
 *    <code>java.lang.Runnable</code></a>.
 *  </p>
 *
 *  @version 0.9.10
 *  @author Philipp Haller
 */
class Reaction extends Runnable {

  private[actors] var a: Actor = _
  private var f: PartialFunction[Any, Unit] = _
  private var msg: Any = _

  def this(a: Actor, f: PartialFunction[Any, Unit], msg: Any) = {
    this()
    this.a = a
    this.f = f
    this.msg = msg
  }

  def this(a: Actor) = this(a, null, null)

  def run() {
    val saved = Actor.tl.get.asInstanceOf[Actor]
    Actor.tl.set(a)
    a.isDetached = false
    try {
      if (a.shouldExit) // links
        a.exit()
      else {
        try {
          if (f == null)
            a.act()
          else
            f(msg)
        } catch {
          case _: KillActorException =>
        }
        a.kill(); a.exit()
      }
    }
    catch {
      case eae: ExitActorException => {
        //Debug.info(a+": exiting...")
        ActorGC.terminated(a)
      }
      case _: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        Debug.info(a+": caught "+t)
        ActorGC.terminated(a)
        // links
        a.synchronized {
          if (!a.links.isEmpty)
            a.exitLinked(t)
          else
            t.printStackTrace()
        }
      }
    } finally {
      Actor.tl.set(saved)
      this.a = null
      this.f = null
      this.msg = null
    }
  }

}
