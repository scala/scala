/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


/**
 * Arrays created by <code>new Array[T](length)</code> where <code>T</code>
 * is a type variable.
 */
[serializable]
final class BoxedAnyArray(val length: Int) extends BoxedArray {

  private var boxed = new Array[Object](length)
  private val hash = boxed.hashCode()
  private var unboxed: Object = null
  private var elemClass: Class = null

  def apply(index: Int): Object = synchronized {
    if (unboxed == null)
      boxed(index);
    else if (elemClass eq ScalaRunTime.IntTYPE)
      BoxedInt.box(unboxed.asInstanceOf[Array[Int]](index))
    else if (elemClass eq ScalaRunTime.DoubleTYPE)
      BoxedDouble.box(unboxed.asInstanceOf[Array[Double]](index))
    else if (elemClass eq ScalaRunTime.FloatTYPE)
      BoxedFloat.box(unboxed.asInstanceOf[Array[Float]](index))
    else if (elemClass eq ScalaRunTime.LongTYPE)
      BoxedLong.box(unboxed.asInstanceOf[Array[Long]](index))
    else if (elemClass eq ScalaRunTime.CharTYPE)
      BoxedChar.box(unboxed.asInstanceOf[Array[Char]](index))
    else if (elemClass eq ScalaRunTime.ByteTYPE)
      BoxedByte.box(unboxed.asInstanceOf[Array[Byte]](index))
    else if (elemClass eq ScalaRunTime.ShortTYPE)
      BoxedShort.box(unboxed.asInstanceOf[Array[Short]](index))
    else if (elemClass eq ScalaRunTime.BooleanTYPE)
      BoxedBoolean.box(unboxed.asInstanceOf[Array[Boolean]](index))
    else
      unboxed.asInstanceOf[Array[Object]](index)
  }

  def update(index: Int, elem: Object): Unit = synchronized {
    if (unboxed == null)
      boxed(index) = elem
    else if (elemClass eq ScalaRunTime.IntTYPE)
      unboxed.asInstanceOf[Array[Int]](index) = elem.asInstanceOf[BoxedNumber].intValue()
    else if (elemClass eq ScalaRunTime.DoubleTYPE)
      unboxed.asInstanceOf[Array[Double]](index) = elem.asInstanceOf[BoxedNumber].doubleValue()
    else if (elemClass eq ScalaRunTime.FloatTYPE)
      unboxed.asInstanceOf[Array[Float]](index) = elem.asInstanceOf[BoxedNumber].floatValue()
    else if (elemClass eq ScalaRunTime.LongTYPE)
      unboxed.asInstanceOf[Array[Long]](index) = elem.asInstanceOf[BoxedNumber].longValue()
    else if (elemClass eq ScalaRunTime.CharTYPE)
      unboxed.asInstanceOf[Array[Char]](index) = elem.asInstanceOf[BoxedNumber].charValue()
    else if (elemClass eq ScalaRunTime.ByteTYPE)
      unboxed.asInstanceOf[Array[Byte]](index) = elem.asInstanceOf[BoxedNumber].byteValue()
    else if (elemClass eq ScalaRunTime.ShortTYPE)
      unboxed.asInstanceOf[Array[Short]](index) = elem.asInstanceOf[BoxedNumber].shortValue()
    else if (elemClass eq ScalaRunTime.BooleanTYPE)
      unboxed.asInstanceOf[Array[Boolean]](index) = elem.asInstanceOf[BoxedBoolean].value
    else
      unboxed.asInstanceOf[Array[Object]](index) = elem
  }

  def unbox(elemTag: String): Object =
    if (elemTag eq ScalaRunTime.IntTag) unbox(ScalaRunTime.IntTYPE)
    else if (elemTag eq ScalaRunTime.DoubleTag) unbox(ScalaRunTime.DoubleTYPE)
    else if (elemTag eq ScalaRunTime.FloatTag) unbox(ScalaRunTime.FloatTYPE)
    else if (elemTag eq ScalaRunTime.LongTag) unbox(ScalaRunTime.LongTYPE)
    else if (elemTag eq ScalaRunTime.CharTag) unbox(ScalaRunTime.CharTYPE)
    else if (elemTag eq ScalaRunTime.ByteTag) unbox(ScalaRunTime.ByteTYPE)
    else if (elemTag eq ScalaRunTime.ShortTag) unbox(ScalaRunTime.ShortTYPE)
    else if (elemTag eq ScalaRunTime.BooleanTag) unbox(ScalaRunTime.BooleanTYPE)
    else unbox(Class.forName(elemTag))

  def unbox(elemClass: Class): Object = synchronized {
    if (unboxed == null) {
      this.elemClass = elemClass;
      if (elemClass eq ScalaRunTime.IntTYPE) {
	val newvalue = new Array[Int](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber]
	  if (x ne null) newvalue(i) = x.intValue();
	  i = i + 1
	}
	unboxed = newvalue
      } else if (elemClass eq ScalaRunTime.DoubleTYPE) {
	val newvalue = new Array[Double](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber];
	  if (x ne null) newvalue(i) = x.doubleValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq ScalaRunTime.FloatTYPE) {
	val newvalue = new Array[Float](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber];
	  if (x ne null) newvalue(i) = x.floatValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq ScalaRunTime.LongTYPE) {
	val newvalue = new Array[Long](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber]
	  if (x ne null) newvalue(i) = x.longValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq ScalaRunTime.CharTYPE) {
	val newvalue = new Array[Char](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber]
	  if (x ne null) newvalue(i) = x.charValue();
	  i = i + 1
	}
	unboxed = newvalue
      } else if (elemClass eq ScalaRunTime.ByteTYPE) {
	val newvalue = new Array[Byte](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber]
	  if (x ne null) newvalue(i) = x.byteValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq ScalaRunTime.ShortTYPE) {
	val newvalue = new Array[Short](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedNumber]
	  if (x ne null) newvalue(i) = x.shortValue();
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq ScalaRunTime.BooleanTYPE) {
	val newvalue = new Array[Boolean](length)
	var i = 0
	while (i < length) {
          val x = boxed(i).asInstanceOf[BoxedBoolean];
	  if (x ne null) newvalue(i) = x.value;
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == boxed.getClass().getComponentType()) {
        // todo: replace with ScalaRunTime.Object.class
	unboxed = boxed
      } else {
	unboxed = java.lang.reflect.Array.newInstance(elemClass, length);
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
            if (ScalaRunTime.isValueClass(that.elemClass)) unbox(that.elemClass);
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
          else if (ScalaRunTime.isValueClass(this.elemClass)) that.unbox(this.elemClass)
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

  override def subArray(start: Int, end: Int): Object = {
    val result = new BoxedAnyArray(end - start);
    Array.copy(this, start, result, 0, end - start)
    result
  }

  override def filter(p: Any => Boolean): Object = {
    val include = new Array[Boolean](length)
    var len = 0
    var i = 0
    while (i < length) {
      if (p(this(i))) { include(i) = true; len = len + 1 }
      i = i + 1
    }
    val result = new BoxedAnyArray(len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = this(i); len = len + 1 }
      i = i + 1
    }
    result
  }
}
