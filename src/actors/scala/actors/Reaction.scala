/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import scala.util.control.ControlException
import java.lang.{InterruptedException, Runnable}

private[actors] class KillActorException extends Throwable with ControlException

/** <p>
 *    The abstract class <code>Reaction</code> associates
 *    an instance of an <code>Actor</code> with a
 *    <a class="java/lang/Runnable" href="" target="contentFrame">
 *    <code>java.lang.Runnable</code></a>.
 *  </p>
 *
 *  @author Philipp Haller
 */
@deprecated
class Reaction(a: Actor, f: PartialFunction[Any, Any], msg: Any) extends ActorTask(a, () => {
  if (f == null)
    a.act()
  else
    f(msg)
}) {

  def this(a: Actor) = this(a, null, null)

}
