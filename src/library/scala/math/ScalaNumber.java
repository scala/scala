/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ScalaNumber.java 19428 2009-11-06 23:59:26Z extempore $


package scala.math;

/** A marker class for Number types introduced by Scala
 *  @author  Martin Odersky, Paul Phillips
 *  @version 2.8
 *  @since 2.8
 */
public abstract class ScalaNumber extends java.lang.Number {
  protected abstract boolean isWhole();
  public abstract Object underlying();
}
