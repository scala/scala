/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Reaction.scala 17862 2009-05-27 23:27:05Z phaller $


package scala.actors

import java.lang.Runnable

/** <p>
 *    The abstract class <code>LightReaction</code> associates
 *    an instance of an <code>OutputChannelActor</code> with a
 *    <a class="java/lang/Runnable" href="" target="contentFrame">
 *    <code>java.lang.Runnable</code></a>.
 *  </p>
 *
 *  @author Philipp Haller
 */
class LightReaction extends Runnable {

  private[actors] var a: OutputChannelActor = _
  private var f: PartialFunction[Any, Unit] = _
  private var msg: Any = _

  def this(a: OutputChannelActor, f: PartialFunction[Any, Unit], msg: Any) = {
    this()
    this.a = a
    this.f = f
    this.msg = msg
  }

  def this(a: OutputChannelActor) = this(a, null, null)

  def run() {
    val saved = Actor.tl.get
    Actor.tl.set(a)
    try {
      try {
        try {
          if (f == null)
            a.act()
          else
            f(msg)
        } catch {
          case e: Exception if (a.exceptionHandler.isDefinedAt(e)) =>
            a.exceptionHandler(e)
        }
      } catch {
        case _: KillActorException =>
      }
      a.kill()
    }
    catch {
      case _: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        Debug.info(a+": caught "+t)
        a.terminated()
      }
    } finally {
      Actor.tl.set(saved)
      this.a = null
      this.f = null
      this.msg = null
    }
  }

}
