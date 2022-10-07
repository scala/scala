/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime

import scala.collection.{AbstractIterable, AbstractIterator, AnyConstr, StrictOptimizedIterableOps, StringOps, StringView, View}
import scala.collection.generic.IsIterable
import scala.collection.immutable.{ArraySeq, NumericRange}
import scala.collection.mutable.StringBuilder
import scala.math.min
import scala.reflect.{ClassTag, classTag}
import scala.util.control.ControlThrowable
import java.lang.{Class => jClass}
import java.lang.reflect.{Method => JMethod}

/** The object ScalaRunTime provides support methods required by
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

  /** Return the class object representing an array with element class `clazz`.
   */
  def arrayClass(clazz: jClass[_]): jClass[_] = {
    // newInstance throws an exception if the erasure is Void.TYPE. see scala/bug#5680
    if (clazz == java.lang.Void.TYPE) classOf[Array[Unit]]
    else java.lang.reflect.Array.newInstance(clazz, 0).getClass
  }

  /** Return the class object representing an unboxed value type,
   *  e.g., classOf[int], not classOf[java.lang.Integer].  The compiler
   *  rewrites expressions like 5.getClass to come here.
   */
  def anyValClass[T <: AnyVal : ClassTag](value: T): jClass[T] =
    classTag[T].runtimeClass.asInstanceOf[jClass[T]]

  /** Retrieve generic array element */
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

  /** Get generic array length */
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

  /** Convert an array to an object array.
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

  /** Verbose formatting. */
  def _toString0(x: Product): String =
    if (x.productArity == 1 && !x.productElement(0).isInstanceOf[Boolean])
      s"${x.productPrefix}(${stringOf(x.productElement(0))})"
    else
      _toString1(x, stringOf(_))

  //x.productElementNames.zip(x.productIterator).map { case (n, v) => s"$n = $v" }.mkString(x.productPrefix + "(", ", ", ")")
  private def _toString1(x: Product, fmt: Any => String): String = {
    val sb = new java.lang.StringBuilder
    val nm = x.productElementNames
    val it = x.productIterator
    sb.append(x.productPrefix).append("(")
    var more = it.hasNext
    while (more) {
      sb.append(nm.next()).append(" = ").append(fmt(it.next()))
      more = it.hasNext
      if (more) sb.append(", ")
    }
    sb.append(")")
    sb.toString
  }

  /** Classic case class toString. */
  def _toString(x: Product): String = x.productIterator.mkString(x.productPrefix + "(", ",", ")")

  def _hashCode(x: Product): Int = scala.util.hashing.MurmurHash3.productHash(x)

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

  // tried to render the string of a recursive structure
  private val BadRecursion = new ControlThrowable {}

  // if they override className to a specific name, they intend to use mkString-style toString
  private lazy val commonCollectionNames = Set("Iterable", "Seq", "Set", "Map")

  private type XMap = scala.collection.Map[_, _]

  /** Max string length produced by `stringOf`, if not specified. */
  val DefaultMaxLength = 1000  // Int.MaxValue

  /** Max collection length processed by `stringOf`, if not specified. */
  val DefaultMaxElements = 1000  // Int.MaxValue

  def stringOf(arg: Any): String = stringOf(arg, maxLength = DefaultMaxLength, verboseProduct = false, maxElements = DefaultMaxElements)
  def stringOf(arg: Any, maxLength: Int): String = stringOf(arg, maxLength, verboseProduct = false, maxElements = DefaultMaxElements)
  def stringOf(arg: Any, maxLength: Int, verboseProduct: Boolean): String = stringOf(arg, maxLength, verboseProduct, maxElements = DefaultMaxElements)

  /** Produce a user-friendly string representation of an arbitrary Scala value.
   *
   *  Handles arrays, null, iterables. If a maxLength is given, then the result
   *  string may be truncated, with an ellipsis showing truncation: `hello, w...`.
   *  A dropped element may be indicated `List(a, b, ...)`.
   *
   *  Does not handle null result of toString. (See replStringOf.)
   *
   *  @param   arg              the value to stringify
   *  @param   maxLength        limit the length of the result string
   *  @param   verboseProduct   extra user-friendly
   *  @param   maxElements      limit the number of elements in a collection
   *  @return  a string representation of arg.
   */
  def stringOf(arg: Any, maxLength: Int, verboseProduct: Boolean, maxElements: Int): String = {
    def packageOf(x: AnyRef) = x.getClass.getPackage match {
      case null   => ""
      case p      => p.getName
    }
    def isScalaClass(x: AnyRef)         = packageOf(x).startsWith("scala.")
    def isScalaCompilerClass(x: AnyRef) = packageOf(x).startsWith("scala.tools.nsc.")

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
    def isXmlImpl(potentialSubClass: Class[_]) =
      isSubClassOf(potentialSubClass, "scala.xml.Node") || isSubClassOf(potentialSubClass, "scala.xml.MetaData")

    // Special casing Unit arrays, the value class which uses a reference array type.
    def arrayToString(x: AnyRef) =
      if (x.getClass.getComponentType == classOf[BoxedUnit])
        stringify((0 until min(array_length(x), maxElements)).iterator, "Array", (_: Int) => "()")
      else
        stringify(x.asInstanceOf[Array[_]].iterator.take(maxElements), "Array", inner)

    // mkString up to maxLength.
    // Format name(a, b, ...). If first element is huge, name(aaa...).
    def stringify[A](it: Iterator[A], name: String, fmt: A => String): String = {
      val limit = maxLength
      val overhead = ", , ...)".length // separator plus trailing ellipsis for next element, if needed
      val sb = new StringBuilder
      sb.append(name).append("(")
      var first = true
      var done = !it.hasNext
      while (!done) {
        val s = fmt(it.next())
        done = limit > 0 && sb.length + s.length + overhead > limit
        if (first) {
          if (done)
            sb.append(s.take(limit - sb.length - 4)).append("...") // partial first element
          else
            sb.append(s)
          first = false
        }
        else {
          if (done)
            sb.append(", ...")  // omit the element
          else
            sb.append(", ").append(s)
        }
        done = done || !it.hasNext
      }
      sb.append(")")
      sb.toString
    }

    /** Format a string as a string literal with escapes and enclosing quotes.
     *  Multiline with escapes gets `s` interpolator.
     */
    def stringToString(s: String): String = {
      import scala.util.Properties.lineSeparator
      var multiline = false
      def escaped = {
        val sub = new StringBuilder
        var i, j = 0
        val len = s.length()
        def addEscapedLineSeparatorChar(n: Int) = {
          var k = 0
          while (k < n) {
            val fill = lineSeparator.charAt(k) match {
              case '\r' => raw"\r"
              case '\n' => raw"\n"
            }
            sub.append(fill)
            k += 1
          }
        }
        while (i < len) {
          val seg = s.charAt(i) match {
            case '\b' => raw"\b"
            case '\f' => raw"\f"
            case '\t' => raw"\t"
            case '\\' => raw"\\"
            case '"'  => "\\\""
            case '\''  => "'"
            case c if c == lineSeparator.charAt(j) =>
              j += 1
              if (j >= lineSeparator.length()) {
                sub.append(lineSeparator)
                j = 0
                multiline = true
              }
              ""
            case '\r' => raw"\r"
            case '\n' => raw"\n"
            case c =>
              if (j > 0) {
                addEscapedLineSeparatorChar(j)
                j = 0
              }
              sub.append(c)
              ""
          }
          seg match {
            case "" => ()
            case _  =>
              if (j > 0) {
                addEscapedLineSeparatorChar(j)
                j = 0
              }
              sub.append(seg)
          }
          i += 1
        }
        if (j > 0) addEscapedLineSeparatorChar(j)
        val res = sub.toString
        if (multiline) s"s${tq}${res}${tq}" else s""""$res""""
      }
      if (s.exists(c => escapable.unapplySeq(c).nonEmpty)) escaped
      else if (s.contains(lineSeparator)) s"${tq}${s}${tq}"
      else s""""$s""""
    }

    // A variation on inner for maps so they print -> instead of bare tuples
    def mapInner(arg: Any): String = arg match {
      case (k, v)   => inner(k) + " -> " + inner(v)
      case _        => inner(arg)
    }

    def safeIterableToString(x: Iterable[_]): String = {
      def safeMapper(y: Any): String =
        if (x eq y.asInstanceOf[AnyRef]) throw BadRecursion
        else inner(y)
      try stringify(x.iterator.take(maxElements), x.collectionClassName, safeMapper)
      catch { case _: BadRecursion.type => x.toString }
    }

    // The recursively applied attempt to prettify Array printing.
    // Note that iterator is used if possible and foreach is used as a
    // last resort, because the parallel collections "foreach" in a
    // random order even on sequences.
    def inner(arg: Any): String = arg match {
      case null                         => "null"
      case ""                           => "\"\""
      case s: String if verboseProduct  => stringToString(s)
      case x: String                    => if (x.head.isWhitespace || x.last.isWhitespace) s""""$x"""" else x
      case x: AnyRef if isArray(x)      => arrayToString(x)
      case
           // Range/NumericRange have a custom toString to avoid walking a gazillion elements
           _: Range | _: NumericRange[_]
           // StringBuilder(a, b, c) and similar not so attractive
         | _: StringView | _: StringOps | _: StringBuilder
           // Don't want to evaluate any elements in a view
         | _: View[_]                   => arg.toString
      // Use toString if not eagerly constructed, to avoid traversing infinity or forcing lazy elements.
      // Custom collections are deemed to have useful toString (scala/bug#3710): either they override className
      // to leverage default toString (mkString) which is always mixed-in, or they override toString or possibly addString.
      // xml.Node and xml.MetaData return themselves in their iterator, so use their xml toString instead.
      // Among other unsavory types, AbstractFile is Iterable[AbstractFile] but not strict, so it will use toString to show name.
      case x: AbstractIterable[_] if x.isTraversableAgain && (
             !x.isInstanceOf[StrictOptimizedIterableOps[_, AnyConstr, _]] ||
             !isScalaClass(x) && commonCollectionNames.contains(x.collectionClassName) ||
             isScalaCompilerClass(x) ||
             isXmlImpl(x.getClass))     => x.toString
      case x: XMap                      => stringify(x.iterator.take(maxElements), x.collectionClassName, mapInner)
      case x: Iterable[_]               => if (x.isTraversableAgain) safeIterableToString(x) else "<iterable>"
      case x: Product1[_] if isTuple(x) => s"(${inner(x._1)},)" // that special trailing comma
      case x: Product if isTuple(x)     => x.productIterator.map(inner).mkString("(", (if (verboseProduct) ", " else ","), ")")
      case x: Product if verboseProduct && x.productArity > 0 =>
        if (x.productArity == 1 && !x.productElement(0).isInstanceOf[Boolean])
          s"${x.productPrefix}(${inner(x.productElement(0))})"
        else
          _toString1(x, inner(_))
      case x                            => x.toString
    }

    // The try/catch is defense against iterables which aren't actually designed
    // to be iterated, such as some scala.tools.nsc.io.AbstractFile derived classes.
    val res =
      try inner(arg)
      catch {
        case _: UnsupportedOperationException | _: AssertionError => arg.toString
      }
    if (res != null && maxLength > 0 && res.length > maxLength) res.take(maxLength - 3) + "..." else res
  } // end stringOf
  private val escapable = raw"""[\x08\f\t\\"']""".r.unanchored
  private final val tq = "\"\"\""

  /** Legacy repl format with extra newlines. */
  def replStringOf(arg: Any, maxLength: Int): String =
    stringOf(arg, maxLength, verboseProduct = false) match {
      case null                      => "null // non-null reference has null-valued toString"
      case s if s.indexOf('\n') >= 0 => "\n" + s + "\n"
      case s                         => s + "\n"
    }

  /** stringOf formatted for use in a repl result, with special case for null. */
  def replStringOf(arg: Any, maxLength: Int, verboseProduct: Boolean): String =
    stringOf(arg, maxLength, verboseProduct) match {
      case null                      => "null // non-null reference has null-valued toString"
      case s                         => s
    }

  // Convert arrays to immutable.ArraySeq for use with Java varargs:
  def genericWrapArray[T](xs: Array[T]): ArraySeq[T] =
    if (xs eq null) null
    else ArraySeq.unsafeWrapArray(xs)
  def wrapRefArray[T <: AnyRef](xs: Array[T]): ArraySeq[T] = {
    if (xs eq null) null
    else if (xs.length == 0) ArraySeq.empty[AnyRef].asInstanceOf[ArraySeq[T]]
    else new ArraySeq.ofRef[T](xs)
  }
  def wrapIntArray(xs: Array[Int]): ArraySeq[Int] = if (xs ne null) new ArraySeq.ofInt(xs) else null
  def wrapDoubleArray(xs: Array[Double]): ArraySeq[Double] = if (xs ne null) new ArraySeq.ofDouble(xs) else null
  def wrapLongArray(xs: Array[Long]): ArraySeq[Long] = if (xs ne null) new ArraySeq.ofLong(xs) else null
  def wrapFloatArray(xs: Array[Float]): ArraySeq[Float] = if (xs ne null) new ArraySeq.ofFloat(xs) else null
  def wrapCharArray(xs: Array[Char]): ArraySeq[Char] = if (xs ne null) new ArraySeq.ofChar(xs) else null
  def wrapByteArray(xs: Array[Byte]): ArraySeq[Byte] = if (xs ne null) new ArraySeq.ofByte(xs) else null
  def wrapShortArray(xs: Array[Short]): ArraySeq[Short] = if (xs ne null) new ArraySeq.ofShort(xs) else null
  def wrapBooleanArray(xs: Array[Boolean]): ArraySeq[Boolean] = if (xs ne null) new ArraySeq.ofBoolean(xs) else null
  def wrapUnitArray(xs: Array[Unit]): ArraySeq[Unit] = if (xs ne null) new ArraySeq.ofUnit(xs) else null
}
