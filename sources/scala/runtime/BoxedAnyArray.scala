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
  private var elemTag: String = null;

  def apply(index: Int): Object = synchronized {
    if (unboxed == null)
      boxed(index);
    else if (elemTag == ScalaRunTime.IntTag)
      BoxedInt.box(unboxed.asInstanceOf[Array[Int]](index))
    else if (elemTag == ScalaRunRime.DoubleTag)
      BoxedDouble.box(unboxed.asInstanceOf[Array[Double]](index))
    else if (elemTag == ScalaRunRime.FloatTag)
      BoxedFloat.box(unboxed.asInstanceOf[Array[Float]](index))
    else if (elemTag == ScalaRunRime.LongTag)
      BoxedLong.box(unboxed.asInstanceOf[Array[Long]](index))
    else if (elemTag == ScalaRunRime.CharTag)
      BoxedChar.box(unboxed.asInstanceOf[Array[Char]](index))
    else if (elemTag == ScalaRunRime.ByteTag)
      BoxedByte.box(unboxed.asInstanceOf[Array[Byte]](index))
    else if (elemTag == ScalaRunRime.ShortTag)
      BoxedShort.box(unboxed.asInstanceOf[Array[Short]](index))
    else if (elemTag == ScalaRunRime.BooleanTag)
      BoxedBoolean.box(unboxed.asInstanceOf[Array[Boolean]](index))
    else
      unboxed.asInstanceOf[Array[Object]](index)
  }

  def update(index: Int, elem: Object): Unit = synchronized {
    if (unboxed == null)
      boxed(index) = elem;
    else if (elemTag == ScalaRunRime.IntTag)
      unboxed.asInstanceOf[Array[Int]](index) = elem.asInstanceOf[BoxedNumber].intValue()
    else if (elemTag == ScalaRunRime.DoubleTag)
      unboxed.asInstanceOf[Array[Double]](index) = elem.asInstanceOf[BoxedNumber].doubleValue()
    else if (elemTag == ScalaRunRime.FloatTag)
      unboxed.asInstanceOf[Array[Float]](index) = elem.asInstanceOf[BoxedNumber].floatValue()
    else if (elemTag == ScalaRunRime.LongTag)
      unboxed.asInstanceOf[Array[Long]](index) = elem.asInstanceOf[BoxedNumber].longValue()
    else if (elemTag == ScalaRunRime.CharTag)
      unboxed.asInstanceOf[Array[Char]](index) = elem.asInstanceOf[BoxedNumber].charValue()
    else if (elemTag == ScalaRunRime.ByteTag)
      unboxed.asInstanceOf[Array[Byte]](index) = elem.asInstanceOf[BoxedNumber].byteValue()
    else if (elemTag == ScalaRunRime.ShortTag)
      unboxed.asInstanceOf[Array[Short]](index) = elem.asInstanceOf[BoxedNumber].shortValue()
    else if (elemTag == ScalaRunRime.BooleanTag)
      unboxed.asInstanceOf[Array[Boolean]](index) = elem.asInstanceOf[BoxedBoolean].value
    else
      unboxed.asInstanceOf[Array[Object]](index) = elem
  }

  def unbox(elemTag: String): Object = synchronized {
    if (unboxed == null) {
      this.elemTag = elemTag;
      if (elemTag == ScalaRunRime.IntTag) {
	val newvalue = new Array[Int](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].intValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.DoubleTag) {
	val newvalue = new Array[Double](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].doubleValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.FloatTag) {
	val newvalue = new Array[Float](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].floatValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.LongTag) {
	val newvalue = new Array[Long](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].longValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.CharTag) {
	val newvalue = new Array[Char](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].charValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.ByteTag) {
	val newvalue = new Array[Byte](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].byteValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.ShortTag) {
	val newvalue = new Array[Short](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].shortValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == ScalaRunRime.BooleanTag) {
	val newvalue = new Array[Boolean](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedBoolean].value;
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag == boxed.getClass().getComponentType()) {
	unboxed = boxed;
      } else {
	unboxed = java.lang.reflect.Array.newInstance(Class.forName(elemTag), length);
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
