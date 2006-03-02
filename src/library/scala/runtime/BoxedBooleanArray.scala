/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


[serializable]
final class BoxedBooleanArray(val value: Array[Boolean]) extends BoxedArray {

  def length: Int = value.length;

  def apply(index: Int): Object = BoxedBoolean.box(value(index));

  def update(index: Int, elem: Object): Unit = {
    value(index) = elem.asInstanceOf[BoxedBoolean].value
  }

  def unbox(elemTag: String): Object = value;

  override def equals(other: Any) = (
    value == other ||
    other.isInstanceOf[BoxedBooleanArray] && value == other.asInstanceOf[BoxedBooleanArray].value
  );

  override def hashCode(): Int = value.hashCode();
}

