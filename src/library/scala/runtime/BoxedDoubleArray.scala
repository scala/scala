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
final class BoxedDoubleArray(val value: Array[Double]) extends BoxedArray {

  def length: Int = value.length;

  def apply(index: Int): Object = BoxedDouble.box(value(index));

  def update(index: Int, elem: Object): Unit = {
    value(index) = elem.asInstanceOf[BoxedNumber].doubleValue()
  }

  def unbox(elemTag: String): Object = value;
  def unbox(elemClass: Class): Object = value;

  override def equals(other: Any) = (
    value == other ||
    other.isInstanceOf[BoxedDoubleArray] && value == other.asInstanceOf[BoxedDoubleArray].value
  );

  override def hashCode(): Int = value.hashCode();

  def subArray(start: Int, end: Int): Array[Double] = {
    val result = new Array[Double](end - start);
    Array.copy(value, 0, result, 0, end - start)
    result
  }

  def filter(p: Any => Boolean): Array[Double] = {
    val include = new Array[Boolean](value.length);
    var len = 0;
    var i = 0;
    while (i < value.length) {
      if (p(value(i))) { include(i) = true; len = len + 1 }
      i = i + 1
    }
    val result = new Array[Double](len);
    len = 0;
    i = 0;
    while (len < result.length) {
      if (include(i)) { result(len) = value(i); len = len + 1 }
      i = i + 1
    }
    result
  }
}

