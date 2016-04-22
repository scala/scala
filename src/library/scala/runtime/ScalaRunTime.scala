/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

import scala.collection.{ TraversableView, AbstractIterator, GenIterable }
import scala.collection.mutable.WrappedArray
import scala.collection.immutable.{ StringLike, NumericRange }
import scala.collection.generic.{ Sorted, IsTraversableLike }
import scala.reflect.{ ClassTag, classTag }
import java.lang.{ Class => jClass }

import java.lang.reflect.{ Method => JMethod }

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
  def drop[Repr](coll: Repr, num: Int)(implicit traversable: IsTraversableLike[Repr]): Repr =
    traversable conversion coll drop num

  /** Return the class object representing an array with element class `clazz`.
   */
  def arrayClass(clazz: jClass[_]): jClass[_] = {
    // newInstance throws an exception if the erasure is Void.TYPE. see SI-5680
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
    xs match {
      case x: Array[AnyRef]  => x(idx).asInstanceOf[Any]
      case x: Array[Int]     => x(idx).asInstanceOf[Any]
      case x: Array[Double]  => x(idx).asInstanceOf[Any]
      case x: Array[Long]    => x(idx).asInstanceOf[Any]
      case x: Array[Float]   => x(idx).asInstanceOf[Any]
      case x: Array[Char]    => x(idx).asInstanceOf[Any]
      case x: Array[Byte]    => x(idx).asInstanceOf[Any]
      case x: Array[Short]   => x(idx).asInstanceOf[Any]
      case x: Array[Boolean] => x(idx).asInstanceOf[Any]
      case x: Array[Unit]    => x(idx).asInstanceOf[Any]
      case null => throw new NullPointerException
    }
  }

  /** update generic array element */
  def array_update(xs: AnyRef, idx: Int, value: Any): Unit = {
    xs match {
      case x: Array[AnyRef]  => x(idx) = value.asInstanceOf[AnyRef]
      case x: Array[Int]     => x(idx) = value.asInstanceOf[Int]
      case x: Array[Double]  => x(idx) = value.asInstanceOf[Double]
      case x: Array[Long]    => x(idx) = value.asInstanceOf[Long]
      case x: Array[Float]   => x(idx) = value.asInstanceOf[Float]
      case x: Array[Char]    => x(idx) = value.asInstanceOf[Char]
      case x: Array[Byte]    => x(idx) = value.asInstanceOf[Byte]
      case x: Array[Short]   => x(idx) = value.asInstanceOf[Short]
      case x: Array[Boolean] => x(idx) = value.asInstanceOf[Boolean]
      case x: Array[Unit]    => x(idx) = value.asInstanceOf[Unit]
      case null => throw new NullPointerException
    }
  }

  /** Get generic array length */
  def array_length(xs: AnyRef): Int = xs match {
    case x: Array[AnyRef]  => x.length
    case x: Array[Int]     => x.length
    case x: Array[Double]  => x.length
    case x: Array[Long]    => x.length
    case x: Array[Float]   => x.length
    case x: Array[Char]    => x.length
    case x: Array[Byte]    => x.length
    case x: Array[Short]   => x.length
    case x: Array[Boolean] => x.length
    case x: Array[Unit]    => x.length
    case null => throw new NullPointerException
  }

  def array_clone(xs: AnyRef): AnyRef = xs match {
    case x: Array[AnyRef]  => x.clone()
    case x: Array[Int]     => x.clone()
    case x: Array[Double]  => x.clone()
    case x: Array[Long]    => x.clone()
    case x: Array[Float]   => x.clone()
    case x: Array[Char]    => x.clone()
    case x: Array[Byte]    => x.clone()
    case x: Array[Short]   => x.clone()
    case x: Array[Boolean] => x.clone()
    case x: Array[Unit]    => x
    case null => throw new NullPointerException
  }

  /** Convert an array to an object array.
   *  Needed to deal with vararg arguments of primitive types that are passed
   *  to a generic Java vararg parameter T ...
   */
  def toObjectArray(src: AnyRef): Array[Object] = src match {
    case x: Array[AnyRef] => x
    case _ =>
      val length = array_length(src)
      val dest = new Array[Object](length)
      for (i <- 0 until length)
        array_update(dest, i, array_apply(src, i))
      dest
  }

  def toArray[T](xs: scala.collection.Seq[T]) = {
    val arr = new Array[AnyRef](xs.length)
    var i = 0
    for (x <- xs) {
      arr(i) = x.asInstanceOf[AnyRef]
      i += 1
    }
    arr
  }

  // Java bug: http://bugs.java.com/bugdatabase/view_bug.do?bug_id=4071957
  // More background at ticket #2318.
  def ensureAccessible(m: JMethod): JMethod = scala.reflect.ensureAccessible(m)

  def _toString(x: Product): String =
    x.productIterator.mkString(x.productPrefix + "(", ",", ")")

  def _hashCode(x: Product): Int = scala.util.hashing.MurmurHash3.productHash(x)

  /** A helper for case classes. */
  def typedProductIterator[T](x: Product): Iterator[T] = {
    new AbstractIterator[T] {
      private var c: Int = 0
      private val cmax = x.productArity
      def hasNext = c < cmax
      def next() = {
        val result = x.productElement(c)
        c += 1
        result.asInstanceOf[T]
      }
    }
  }

  /** Old implementation of `##`. */
  @deprecated("Use scala.runtime.Statics.anyHash instead.", "2.12.0")
  def hash(x: Any): Int = Statics.anyHash(x.asInstanceOf[Object])

  /** Given any Scala value, convert it to a String.
   *
   * The primary motivation for this method is to provide a means for
   * correctly obtaining a String representation of a value, while
   * avoiding the pitfalls of naively calling toString on said value.
   * In particular, it addresses the fact that (a) toString cannot be
   * called on null and (b) depending on the apparent type of an
   * array, toString may or may not print it in a human-readable form.
   *
   * @param   arg   the value to stringify
   * @return        a string representation of arg.
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
      case _: Sorted[_, _]  => true
      // StringBuilder(a, b, c) and similar not so attractive
      case _: StringLike[_] => true
      // Don't want to evaluate any elements in a view
      case _: TraversableView[_, _] => true
      // Node extends NodeSeq extends Seq[Node] and MetaData extends Iterable[MetaData]
      // -> catch those by isXmlNode and isXmlMetaData.
      // Don't want to a) traverse infinity or b) be overly helpful with peoples' custom
      // collections which may have useful toString methods - ticket #3710
      // or c) print AbstractFiles which are somehow also Iterable[AbstractFile]s.
      case x: Traversable[_] => !x.hasDefiniteSize || !isScalaClass(x) || isScalaCompilerClass(x) || isXmlNode(x.getClass) || isXmlMetaData(x.getClass)
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
        0 until (array_length(x) min maxElements) map (_ => "()") mkString ("Array(", ", ", ")")
      else
        WrappedArray make x take maxElements map inner mkString ("Array(", ", ", ")")
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
      case x: scala.collection.Map[_, _]      => x.iterator take maxElements map mapInner mkString (x.stringPrefix + "(", ", ", ")")
      case x: GenIterable[_]            => x.iterator take maxElements map inner mkString (x.stringPrefix + "(", ", ", ")")
      case x: Traversable[_]            => x take maxElements map inner mkString (x.stringPrefix + "(", ", ", ")")
      case x: Product1[_] if isTuple(x) => "(" + inner(x._1) + ",)" // that special trailing comma
      case x: Product if isTuple(x)     => x.productIterator map inner mkString ("(", ",", ")")
      case x                            => x.toString
    }

    // The try/catch is defense against iterables which aren't actually designed
    // to be iterated, such as some scala.tools.nsc.io.AbstractFile derived classes.
    try inner(arg)
    catch {
      case _: UnsupportedOperationException | _: AssertionError => "" + arg
    }
  }

  /** stringOf formatted for use in a repl result. */
  def replStringOf(arg: Any, maxElements: Int): String = {
    val s  = stringOf(arg, maxElements)
    val nl = if (s contains "\n") "\n" else ""

    nl + s + "\n"
  }
}
