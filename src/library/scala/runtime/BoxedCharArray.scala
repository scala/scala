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
final class BoxedCharArray(val value: Array[Char]) extends BoxedArray {

  def length: Int = value.length;

  def apply(index: Int): Object = BoxedChar.box(value(index));

  def update(index: Int, elem: Object): Unit = {
    value(index) = elem.asInstanceOf[BoxedNumber].charValue()
  }

  def unbox(elemTag: String): Object = value;

  override def equals(other: Any) = (
    value == other ||
    other.isInstanceOf[BoxedCharArray] && value == other.asInstanceOf[BoxedCharArray].value
  );

  override def hashCode(): Int = value.hashCode();
}

