/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.runtime;

/** Arrays created by    new Array[T](length)    where T is a type variable
 */
final class BoxedAnyArray(val length: Int) extends BoxedArray {

  private var boxed = new Array[Object](length);
  private val hash = boxed.hashCode();
  private var unboxed: Object = null;
  private var elemClass: Class = null;

  def apply(index: Int): Object = synchronized {
    if (unboxed == null)
      boxed(index);
    else if (elemClass == java.lang.Integer.TYPE)
      BoxedInt.box(unboxed.asInstanceOf[Array[Int]](index))
    else if (elemClass == java.lang.Double.TYPE)
      BoxedDouble.box(unboxed.asInstanceOf[Array[Double]](index))
    else if (elemClass == java.lang.Float.TYPE)
      BoxedFloat.box(unboxed.asInstanceOf[Array[Float]](index))
    else if (elemClass == java.lang.Long.TYPE)
      BoxedLong.box(unboxed.asInstanceOf[Array[Long]](index))
    else if (elemClass == java.lang.Character.TYPE)
      BoxedChar.box(unboxed.asInstanceOf[Array[Char]](index))
    else if (elemClass == java.lang.Byte.TYPE)
      BoxedByte.box(unboxed.asInstanceOf[Array[Byte]](index))
    else if (elemClass == java.lang.Short.TYPE)
      BoxedShort.box(unboxed.asInstanceOf[Array[Short]](index))
    else if (elemClass == java.lang.Boolean.TYPE)
      BoxedBoolean.box(unboxed.asInstanceOf[Array[Boolean]](index))
    else
      unboxed.asInstanceOf[Array[Object]](index)
  }

  def update(index: Int, elem: Object): Unit = synchronized {
    if (unboxed == null)
      boxed(index) = elem;
    else if (elemClass == java.lang.Integer.TYPE)
      unboxed.asInstanceOf[Array[Int]](index) = elem.asInstanceOf[BoxedNumber].intValue()
    else if (elemClass == java.lang.Double.TYPE)
      unboxed.asInstanceOf[Array[Double]](index) = elem.asInstanceOf[BoxedNumber].doubleValue()
    else if (elemClass == java.lang.Float.TYPE)
      unboxed.asInstanceOf[Array[Float]](index) = elem.asInstanceOf[BoxedNumber].floatValue()
    else if (elemClass == java.lang.Long.TYPE)
      unboxed.asInstanceOf[Array[Long]](index) = elem.asInstanceOf[BoxedNumber].longValue()
    else if (elemClass == java.lang.Character.TYPE)
      unboxed.asInstanceOf[Array[Char]](index) = elem.asInstanceOf[BoxedNumber].charValue()
    else if (elemClass == java.lang.Byte.TYPE)
      unboxed.asInstanceOf[Array[Byte]](index) = elem.asInstanceOf[BoxedNumber].byteValue()
    else if (elemClass == java.lang.Short.TYPE)
      unboxed.asInstanceOf[Array[Short]](index) = elem.asInstanceOf[BoxedNumber].shortValue()
    else if (elemClass == java.lang.Boolean.TYPE)
      unboxed.asInstanceOf[Array[Boolean]](index) = elem.asInstanceOf[BoxedBoolean].value
    else
      unboxed.asInstanceOf[Array[Object]](index) = elem
  }

  def unbox(elemClass: Class): Object = synchronized {
    if (unboxed == null) {
      this.elemClass = elemClass;
      if (elemClass == java.lang.Integer.TYPE) {
	val newvalue = new Array[Int](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].intValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Double.TYPE) {
	val newvalue = new Array[Double](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].doubleValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Float.TYPE) {
	val newvalue = new Array[Float](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].floatValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Long.TYPE) {
	val newvalue = new Array[Long](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].longValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Character.TYPE) {
	val newvalue = new Array[Char](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].charValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Byte.TYPE) {
	val newvalue = new Array[Byte](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].byteValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Short.TYPE) {
	val newvalue = new Array[Short](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].shortValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == java.lang.Boolean.TYPE) {
	val newvalue = new Array[Boolean](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedBoolean].value;
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == boxed.getClass().getComponentType()) {
	unboxed = boxed;
      } else {
	unboxed = java.lang.reflect.Array.newInstance(elemClass, length);
	System.arraycopy(boxed, 0, unboxed, 0, length);
      }
      boxed = null
    }
    unboxed
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[BoxedAnyArray] && (this eq (other.asInstanceOf[BoxedAnyArray])) ||
    (if (unboxed == null) boxed == other else unboxed == other);

  override def hashCode(): Int = hash;
}
