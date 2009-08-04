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
 *    an instance of a <code>Reactor</code> with a
 *    <a class="java/lang/Runnable" href="" target="contentFrame">
 *    <code>java.lang.Runnable</code></a>.
 *  </p>
 *
 *  @author Philipp Haller
 */
private[actors] class LightReaction(a: Reactor, f: PartialFunction[Any, Unit], msg: Any) extends ReactorTask(a, () => {
  if (f == null)
    a.act()
  else
    f(msg)
}) {

  def this(a: Reactor) = this(a, null, null)

}
