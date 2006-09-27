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
 * Function with 1 parameter. In the following example the definition of
 * <code>succ</code> is a shorthand for the anonymous class definition
 * <code>anonfun1</code>:
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>val</b> succ = (x: Int) => x + 1
 *
 *   <b>val</b> anonfun1 = <b>new</b> Function1[Int, Int] {
 *     <b>def</b> apply(x: Int): Int = x + 1
 *   }
 *
 *   Console.println(succ(0))
 *   Console.println(anonfun1(0))
 * }</pre>
 */
trait Function1[-T0, +R] extends AnyRef {
  def apply(v0: T0): R
  override def toString() = "<function>"
  def compose[A](g: A => T0): A => R = { x => apply(g(x)) }
}

