/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime

import scala.collection.{AbstractIterator, AnyConstr, SortedOps, StrictOptimizedIterableOps, StringOps, StringView, View}
import scala.collection.generic.IsIterable
import scala.collection.immutable.{ArraySeq, NumericRange}
import scala.collection.mutable.StringBuilder
import scala.math.min
import scala.reflect.{ClassTag, classTag}
import java.lang.{Class => jClass}
import java.lang.reflect.{Method => JMethod}

/** The object `ScalaRunTime` provides support methods required by
 *  the scala runtime.  All these methods should be considered
 *  outside the API and subject to change or removal without notice.
 */
object ScalaRunTime {
  def isArray(x: Any, atLevel: Int = 1): Boolean =
    x != null && isArrayClass(x.getClass, atLevel)

  private def isArrayClass(clazz: jClass[_], atLevel: Int): Boolean =
    clazz.isArray && (atLevel == 1 || isArrayClass(clazz.getComponentType, atLevel - 1))

  // A helper method to make my life in the pattern matcher a lot easier.
  def drop[Repr](coll: Repr, num: Int)(implicit iterable: IsIterable[Repr] { type C <: Repr }): Repr =
    iterable(coll) drop num

  /** Returns the class object representing an array with element class `clazz`.
   */
  def arrayClass(clazz: jClass[_]): jClass[_] = {
    // newInstance throws an exception if the erasure is Void.TYPE. see scala/bug#5680
    if (clazz == java.lang.Void.TYPE) classOf[Array[Unit]]
    else java.lang.reflect.Array.newInstance(clazz, 0).getClass
  }

  /** Returns the class object representing an unboxed value type,
   *  e.g., classOf[int], not classOf[java.lang.Integer].  The compiler
   *  rewrites expressions like 5.getClass to come here.
   */
  def anyValClass[T <: AnyVal : ClassTag](value: T): jClass[T] =
    classTag[T].runtimeClass.asInstanceOf[jClass[T]]

  /** Retrieves generic array element */
  def array_apply(xs: AnyRef, idx: Int): Any = {
    (xs: @unchecked) match {
      case x: Array[AnyRef]  => x(idx).asInstanceOf[Any]
      case x: Array[Int]     => x(idx).asInstanceOf[Any]
      case x: Array[Double]  => x(idx).asInstanceOf[Any]
      case x: Array[Long]    => x(idx).asInstanceOf[Any]
      case x: Array[Float]   => x(idx).asInstanceOf[Any]
      case x: Array[Char]    => x(idx).asInstanceOf[Any]
      case x: Array[Byte]    => x(idx).asInstanceOf[Any]
      case x: Array[Short]   => x(idx).asInstanceOf[Any]
      case x: Array[Boolean] => x(idx).asInstanceOf[Any]
      case null => throw new NullPointerException
    }
  }

  /** update generic array element */
  def array_update(xs: AnyRef, idx: Int, value: Any): Unit = {
    (xs: @unchecked) match {
      case x: Array[AnyRef]  => x(idx) = value.asInstanceOf[AnyRef]
      case x: Array[Int]     => x(idx) = value.asInstanceOf[Int]
      case x: Array[Double]  => x(idx) = value.asInstanceOf[Double]
      case x: Array[Long]    => x(idx) = value.asInstanceOf[Long]
      case x: Array[Float]   => x(idx) = value.asInstanceOf[Float]
      case x: Array[Char]    => x(idx) = value.asInstanceOf[Char]
      case x: Array[Byte]    => x(idx) = value.asInstanceOf[Byte]
      case x: Array[Short]   => x(idx) = value.asInstanceOf[Short]
      case x: Array[Boolean] => x(idx) = value.asInstanceOf[Boolean]
      case null => throw new NullPointerException
    }
  }

  /** Gets generic array length */
  @inline def array_length(xs: AnyRef): Int = java.lang.reflect.Array.getLength(xs)

  // TODO: bytecode Object.clone() will in fact work here and avoids
  // the type switch. See Array_clone comment in BCodeBodyBuilder.
  def array_clone(xs: AnyRef): AnyRef = (xs: @unchecked) match {
    case x: Array[AnyRef]  => x.clone()
    case x: Array[Int]     => x.clone()
    case x: Array[Double]  => x.clone()
    case x: Array[Long]    => x.clone()
    case x: Array[Float]   => x.clone()
    case x: Array[Char]    => x.clone()
    case x: Array[Byte]    => x.clone()
    case x: Array[Short]   => x.clone()
    case x: Array[Boolean] => x.clone()
    case null => throw new NullPointerException
  }

  /** Converts an array to an object array.
   *  Needed to deal with vararg arguments of primitive types that are passed
   *  to a generic Java vararg parameter T ...
   */
  def toObjectArray(src: AnyRef): Array[Object] = {
    def copy[@specialized T <: AnyVal](src: Array[T]): Array[Object] = {
      val length = src.length
      if (length == 0) Array.emptyObjectArray
      else {
        val dest = new Array[Object](length)
        var i = 0
        while (i < length) {
          dest(i) = src(i).asInstanceOf[AnyRef]
          i += 1
        }
        dest
      }
    }
    (src: @unchecked) match {
      case x: Array[AnyRef]  => x
      case x: Array[Int]     => copy(x)
      case x: Array[Double]  => copy(x)
      case x: Array[Long]    => copy(x)
      case x: Array[Float]   => copy(x)
      case x: Array[Char]    => copy(x)
      case x: Array[Byte]    => copy(x)
      case x: Array[Short]   => copy(x)
      case x: Array[Boolean] => copy(x)
      case null => throw new NullPointerException
    }
  }

  def toArray[T](xs: scala.collection.Seq[T]) = {
    if (xs.isEmpty) Array.emptyObjectArray
    else {
      val arr = new Array[AnyRef](xs.length)
      val it = xs.iterator
      var i = 0
      while (it.hasNext) {
        arr(i) = it.next().asInstanceOf[AnyRef]
        i += 1
      }
      arr
    }
  }

  // Java bug: https://bugs.java.com/view_bug.do?bug_id=4071957
  // More background at ticket #2318.
  def ensureAccessible(m: JMethod): JMethod = scala.reflect.ensureAccessible(m)

  // This is called by the synthetic case class `toString` method.
  // It originally had a `CaseClass` parameter type which was changed to `Product`.
  def _toString(x: Product): String =
    x.productIterator.mkString(x.productPrefix + "(", ",", ")")

  // This method is called by case classes compiled by older Scala 2.13 / Scala 3 versions, so it needs to stay.
  // In newer versions, the synthetic case class `hashCode` has either the calculation inlined or calls
  // `MurmurHash3.productHash`.
  // There used to be an `_equals` method as well which was removed in 5e7e81ab2a.
  def _hashCode(x: Product): Int = scala.util.hashing.MurmurHash3.caseClassHash(x)

  /** A helper for case classes. */
  def typedProductIterator[T](x: Product): Iterator[T] = {
    new AbstractIterator[T] {
      private[this] var c: Int = 0
      private[this] val cmax = x.productArity
      def hasNext = c < cmax
      def next() = {
        val result = x.productElement(c)
        c += 1
        result.asInstanceOf[T]
      }
    }
  }

  /** Given any Scala value, converts it to a `String`.
   *
   * The primary motivation for this method is to provide a means for
   * correctly obtaining a `String` representation of a value, while
   * avoiding the pitfalls of naively calling `toString` on said value.
   * In particular, it addresses the fact that (a) `toString` cannot be
   * called on `null` and (b) depending on the apparent type of an
   * array, `toString` may or may not print it in a human-readable form.
   *
   * @param   arg   the value to stringify
   * @return        a string representation of `arg`.
   */
  def stringOf(arg: Any): String = stringOf(arg, scala.Int.MaxValue)
  def stringOf(arg: Any, maxElements: Int): String = {
    def packageOf(x: AnyRef) = x.getClass.getPackage match {
      case null   => ""
      case p      => p.getName
    }
    def isScalaClass(x: AnyRef)         = packageOf(x) startsWith "scala."
    def isScalaCompilerClass(x: AnyRef) = packageOf(x) startsWith "scala.tools.nsc."

    // includes specialized subclasses and future proofed against hypothetical TupleN (for N > 22)
    def isTuple(x: Any) = x != null && x.getClass.getName.startsWith("scala.Tuple")

    // We use reflection because the scala.xml package might not be available
    def isSubClassOf(potentialSubClass: Class[_], ofClass: String) =
      try {
        val classLoader = potentialSubClass.getClassLoader
        val clazz = Class.forName(ofClass, /*initialize =*/ false, classLoader)
        clazz.isAssignableFrom(potentialSubClass)
      } catch {
        case cnfe: ClassNotFoundException => false
      }
    def isXmlNode(potentialSubClass: Class[_])     = isSubClassOf(potentialSubClass, "scala.xml.Node")
    def isXmlMetaData(potentialSubClass: Class[_]) = isSubClassOf(potentialSubClass, "scala.xml.MetaData")

    // When doing our own iteration is dangerous
    def useOwnToString(x: Any) = x match {
      // Range/NumericRange have a custom toString to avoid walking a gazillion elements
      case _: Range | _: NumericRange[_] => true
      // Sorted collections to the wrong thing (for us) on iteration - ticket #3493
      case _: SortedOps[_, _]  => true
      // StringBuilder(a, b, c) and similar not so attractive
      case _: StringView | _: StringOps | _: StringBuilder => true
      // Don't want to evaluate any elements in a view
      case _: View[_] => true
      // Node extends NodeSeq extends Seq[Node] and MetaData extends Iterable[MetaData]
      // -> catch those by isXmlNode and isXmlMetaData.
      // Don't want to a) traverse infinity or b) be overly helpful with peoples' custom
      // collections which may have useful toString methods - ticket #3710
      // or c) print AbstractFiles which are somehow also Iterable[AbstractFile]s.
      case x: Iterable[_] => (!x.isInstanceOf[StrictOptimizedIterableOps[_, AnyConstr, _]]) || !isScalaClass(x) || isScalaCompilerClass(x) || isXmlNode(x.getClass) || isXmlMetaData(x.getClass)
      // Otherwise, nothing could possibly go wrong
      case _ => false
    }

    // A variation on inner for maps so they print -> instead of bare tuples
    def mapInner(arg: Any): String = arg match {
      case (k, v)   => inner(k) + " -> " + inner(v)
      case _        => inner(arg)
    }

    // Special casing Unit arrays, the value class which uses a reference array type.
    def arrayToString(x: AnyRef) = {
      if (x.getClass.getComponentType == classOf[BoxedUnit])
        (0 until min(array_length(x), maxElements)).map(_ => "()").mkString("Array(", ", ", ")")
      else
        x.asInstanceOf[Array[_]].iterator.take(maxElements).map(inner).mkString("Array(", ", ", ")")
    }

    // The recursively applied attempt to prettify Array printing.
    // Note that iterator is used if possible and foreach is used as a
    // last resort, because the parallel collections "foreach" in a
    // random order even on sequences.
    def inner(arg: Any): String = arg match {
      case null                         => "null"
      case ""                           => "\"\""
      case x: String                    => if (x.head.isWhitespace || x.last.isWhitespace) "\"" + x + "\"" else x
      case x if useOwnToString(x)       => x.toString
      case x: AnyRef if isArray(x)      => arrayToString(x)
      case x: scala.collection.Map[_, _] => x.iterator.take(maxElements).map(mapInner).mkString(x.collectionClassName + "(", ", ", ")")
      case x: Iterable[_]               => x.iterator.take(maxElements).map(inner).mkString(x.collectionClassName + "(", ", ", ")")
      case x: Product1[_] if isTuple(x) => "(" + inner(x._1) + ",)" // that special trailing comma
      case x: Product if isTuple(x)     => x.productIterator.map(inner).mkString("(", ",", ")")
      case x                            => x.toString
    }

    // The try/catch is defense against iterables which aren't actually designed
    // to be iterated, such as some scala.tools.nsc.io.AbstractFile derived classes.
    try inner(arg)
    catch {
      case _: UnsupportedOperationException | _: AssertionError => "" + arg
    }
  }

  /** `stringOf` formatted for use in a repl result. */
  def replStringOf(arg: Any, maxElements: Int): String =
    stringOf(arg, maxElements) match {
      case null => "null toString"
      case s if s.indexOf('\n') >= 0 => "\n" + s + "\n"
      case s => s + "\n"
    }

  // Convert an array to an immutable.ArraySeq for use with Scala varargs.
  // `foo(x, y)` is compiled to `foo(wrapXArray(Array(x, y))).
  // For `foo()`, the Scala 2 compiler uses a reference to `Nil` instead; Scala 3 doesn't have this special case.
  //
  // The `null` checks are there for backwards compatibility. They were removed in 2.13.17 (scala/scala#11021)
  // which led to a ticket in Scala 3 (scala/scala3#24204). The argument may be null:
  //   - When calling a Scala `@varargs` method from Java
  //   - When using an array as sequence argument in Scala 3: `foo((null: Array[X])*)`
  def genericWrapArray[T](xs: Array[T]): ArraySeq[T]          = if (xs ne null) ArraySeq.unsafeWrapArray(xs) else null
  def wrapRefArray[T <: AnyRef](xs: Array[T]): ArraySeq[T]    = if (xs ne null) new ArraySeq.ofRef[T](xs) else null
  def wrapIntArray(xs: Array[Int]): ArraySeq[Int]             = if (xs ne null) new ArraySeq.ofInt(xs) else null
  def wrapDoubleArray(xs: Array[Double]): ArraySeq[Double]    = if (xs ne null) new ArraySeq.ofDouble(xs) else null
  def wrapLongArray(xs: Array[Long]): ArraySeq[Long]          = if (xs ne null) new ArraySeq.ofLong(xs) else null
  def wrapFloatArray(xs: Array[Float]): ArraySeq[Float]       = if (xs ne null) new ArraySeq.ofFloat(xs) else null
  def wrapCharArray(xs: Array[Char]): ArraySeq[Char]          = if (xs ne null) new ArraySeq.ofChar(xs) else null
  def wrapByteArray(xs: Array[Byte]): ArraySeq[Byte]          = if (xs ne null) new ArraySeq.ofByte(xs) else null
  def wrapShortArray(xs: Array[Short]): ArraySeq[Short]       = if (xs ne null) new ArraySeq.ofShort(xs) else null
  def wrapBooleanArray(xs: Array[Boolean]): ArraySeq[Boolean] = if (xs ne null) new ArraySeq.ofBoolean(xs) else null
  def wrapUnitArray(xs: Array[Unit]): ArraySeq[Unit]          = if (xs ne null) new ArraySeq.ofUnit(xs) else null
}
