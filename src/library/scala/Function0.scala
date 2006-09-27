/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


/**
 * Function with no parameters. In the following example the definition of
 * <code>currentSeconds</code> is a shorthand for the anonymous class
 * definition <code>anonfun0</code>:
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>val</b> currentSeconds = () => System.currentTimeMillis() / 1000L
 *
 *   <b>val</b> anonfun0 = <b>new</b> Function0[Long] {
 *     <b>def</b> apply(): Long = System.currentTimeMillis() / 1000L
 *   }
 *
 *   Console.println(currentSeconds())
 *   Console.println(anonfun0())
 * }</pre>
 */
trait Function0[+R] extends AnyRef {
  def apply(): R
  override def toString() = "<function>"
}
