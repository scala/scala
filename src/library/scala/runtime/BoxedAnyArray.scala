/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


/**
 * Arrays created by    new Array[T](length)    where T is a type variable
 */
[serializable]
final class BoxedAnyArray(val length: Int) extends BoxedArray {

  private var boxed = new Array[Object](length);
  private val hash = boxed.hashCode();
  private var unboxed: Object = null;
  private var elemTag: String = null;

  def apply(index: Int): Object = synchronized {
    if (unboxed == null)
      boxed(index);
    else if (elemTag eq ScalaRunTime.IntTag)
      BoxedInt.box(unboxed.asInstanceOf[Array[Int]](index))
    else if (elemTag eq ScalaRunTime.DoubleTag)
      BoxedDouble.box(unboxed.asInstanceOf[Array[Double]](index))
    else if (elemTag eq ScalaRunTime.FloatTag)
      BoxedFloat.box(unboxed.asInstanceOf[Array[Float]](index))
    else if (elemTag eq ScalaRunTime.LongTag)
      BoxedLong.box(unboxed.asInstanceOf[Array[Long]](index))
    else if (elemTag eq ScalaRunTime.CharTag)
      BoxedChar.box(unboxed.asInstanceOf[Array[Char]](index))
    else if (elemTag eq ScalaRunTime.ByteTag)
      BoxedByte.box(unboxed.asInstanceOf[Array[Byte]](index))
    else if (elemTag eq ScalaRunTime.ShortTag)
      BoxedShort.box(unboxed.asInstanceOf[Array[Short]](index))
    else if (elemTag eq ScalaRunTime.BooleanTag)
      BoxedBoolean.box(unboxed.asInstanceOf[Array[Boolean]](index))
    else
      unboxed.asInstanceOf[Array[Object]](index)
  }

  def update(index: Int, elem: Object): Unit = synchronized {
    if (unboxed == null)
      boxed(index) = elem;
    else if (elemTag eq ScalaRunTime.IntTag)
      unboxed.asInstanceOf[Array[Int]](index) = elem.asInstanceOf[BoxedNumber].intValue()
    else if (elemTag eq ScalaRunTime.DoubleTag)
      unboxed.asInstanceOf[Array[Double]](index) = elem.asInstanceOf[BoxedNumber].doubleValue()
    else if (elemTag eq ScalaRunTime.FloatTag)
      unboxed.asInstanceOf[Array[Float]](index) = elem.asInstanceOf[BoxedNumber].floatValue()
    else if (elemTag eq ScalaRunTime.LongTag)
      unboxed.asInstanceOf[Array[Long]](index) = elem.asInstanceOf[BoxedNumber].longValue()
    else if (elemTag eq ScalaRunTime.CharTag)
      unboxed.asInstanceOf[Array[Char]](index) = elem.asInstanceOf[BoxedNumber].charValue()
    else if (elemTag eq ScalaRunTime.ByteTag)
      unboxed.asInstanceOf[Array[Byte]](index) = elem.asInstanceOf[BoxedNumber].byteValue()
    else if (elemTag eq ScalaRunTime.ShortTag)
      unboxed.asInstanceOf[Array[Short]](index) = elem.asInstanceOf[BoxedNumber].shortValue()
    else if (elemTag eq ScalaRunTime.BooleanTag)
      unboxed.asInstanceOf[Array[Boolean]](index) = elem.asInstanceOf[BoxedBoolean].value
    else
      unboxed.asInstanceOf[Array[Object]](index) = elem
  }

  def unbox(elemTag: String): Object = synchronized {
    if (unboxed == null) {
      this.elemTag = elemTag;
      if (elemTag eq ScalaRunTime.IntTag) {
	val newvalue = new Array[Int](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].intValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.DoubleTag) {
	val newvalue = new Array[Double](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].doubleValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.FloatTag) {
	val newvalue = new Array[Float](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].floatValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.LongTag) {
	val newvalue = new Array[Long](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].longValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.CharTag) {
	val newvalue = new Array[Char](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].charValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.ByteTag) {
	val newvalue = new Array[Byte](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].byteValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.ShortTag) {
	val newvalue = new Array[Short](length);
	var i = 0;
	while (i < length) {
	  newvalue(i) = boxed(i).asInstanceOf[BoxedNumber].shortValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemTag eq ScalaRunTime.BooleanTag) {
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

  override def equals(other: Any): Boolean = (
    other.isInstanceOf[BoxedAnyArray] && (this eq (other.asInstanceOf[BoxedAnyArray])) ||
    (if (unboxed == null) boxed == other else unboxed == other)
  )

  override def hashCode(): Int = hash

  def value: Object = {
    if (unboxed == null) throw new NotDefinedError("BoxedAnyArray.value")
    unboxed
  }

  private def adapt(other: Object): Object =
    if (this.unboxed == null)
      other match {
        case that: BoxedAnyArray =>
          if (that.unboxed == null) {
            that.boxed
          } else {
            if (ScalaRunTime.isValueTag(that.elemTag)) unbox(that.elemTag);
            that.unboxed
          }
        case that: BoxedArray =>
          adapt(that.value)
        case that: Array[Int] =>
          unbox(ScalaRunTime.IntTag); that
        case that: Array[Double] =>
          unbox(ScalaRunTime.DoubleTag); that
        case that: Array[Float] =>
          unbox(ScalaRunTime.FloatTag); that
        case that: Array[Long] =>
          unbox(ScalaRunTime.LongTag); that
        case that: Array[Char] =>
          unbox(ScalaRunTime.CharTag); that
        case that: Array[Short] =>
          unbox(ScalaRunTime.ShortTag); that
        case that: Array[Byte] =>
          unbox(ScalaRunTime.ByteTag); that
        case that: Array[Boolean] =>
          unbox(ScalaRunTime.BooleanTag); that
        case _ =>
          other
      }
    else
      other match {
        case that: BoxedAnyArray =>
          if (that.unboxed != null) that.unboxed
          else if (ScalaRunTime.isValueTag(this.elemTag)) that.unbox(this.elemTag)
          else that.boxed
        case that: BoxedArray =>
          adapt(that.value)
        case _ =>
          other
      }

  override def copyFrom(src: Object, from: Int, to: Int, len: Int): Unit = {
    val src1 = adapt(src)
    Array.copy(src1, from, if (unboxed != null) unboxed else boxed, to, len)
  }

  override def copyTo(from: Int, dest: Object, to: Int, len: Int): Unit = {
    var dest1 = adapt(dest)
    Array.copy(if (unboxed != null) unboxed else boxed, from, dest1, to, len)
  }
}
