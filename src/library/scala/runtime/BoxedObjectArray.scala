/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

final class BoxedObjectArray(val value: Array[Object]) extends BoxedArray {

  def length: Int = value.length;

  def apply(index: Int): Object = value(index);

  def update(index: Int, elem: Object): Unit = { value(index) = elem }

  def unbox(elemTag: String): Object = value;

  override def equals(other: Any): Boolean = (
    value == other ||
    other.isInstanceOf[BoxedObjectArray] && value == other.asInstanceOf[BoxedObjectArray].value
  );

  override def hashCode(): Int = value.hashCode();
}
