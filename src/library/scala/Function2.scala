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
 * Function with 2 parameters. In the following example the definition of
 * <code>max</code> is a shorthand for the anonymous class definition
 * <code>anonfun2</code>:
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>val</b> max = (x: Int, y: Int) => <b>if</b> (x < y) y <b>else</b> x
 *
 *   <b>val</b> anonfun2 = <b>new</b> Function2[Int, Int, Int] {
 *     <b>def</b> apply(x: Int, y: Int): Int = <b>if</b> (x < y) y <b>else</b> x
 *   }
 *
 *   Console.println(max(0, 1))
 *   Console.println(anonfun2(0, 1))
 * }</pre>
 */
trait Function2[-T0, -T1, +R] extends AnyRef {
  def apply(v0: T0, v1: T1): R
  override def toString() = "<function>"
}
