/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


/* INSTRUMENTED VERSION */


package scala.runtime

import scala.reflect.ClassManifest
import scala.collection.{ Seq, IndexedSeq, TraversableView }
import scala.collection.mutable.WrappedArray
import scala.collection.immutable.{ NumericRange, List, Stream, Nil, :: }
import scala.collection.generic.{ Sorted }
import scala.xml.{ Node, MetaData }
import scala.util.control.ControlThrowable
import java.lang.reflect.{ Modifier, Method => JMethod }

/* The object <code>ScalaRunTime</code> provides ...
 */
object ScalaRunTime {
  def isArray(x: AnyRef): Boolean = isArray(x, 1)
  def isArray(x: Any, atLevel: Int): Boolean = 
    x != null && isArrayClass(x.asInstanceOf[AnyRef].getClass, atLevel)

  private def isArrayClass(clazz: Class[_], atLevel: Int): Boolean =
    clazz.isArray && (atLevel == 1 || isArrayClass(clazz.getComponentType, atLevel - 1))

  def isValueClass(clazz: Class[_]) = clazz.isPrimitive() 
  
  var arrayApplyCount = 0
  var arrayUpdateCount = 0
  
  /** Retrieve generic array element */
  def array_apply(xs: AnyRef, idx: Int): Any = {
    arrayApplyCount += 1
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
    arrayUpdateCount += 1
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
    case x: Array[AnyRef]  => ArrayRuntime.cloneArray(x)
    case x: Array[Int]     => ArrayRuntime.cloneArray(x)
    case x: Array[Double]  => ArrayRuntime.cloneArray(x)
    case x: Array[Long]    => ArrayRuntime.cloneArray(x)
    case x: Array[Float]   => ArrayRuntime.cloneArray(x)
    case x: Array[Char]    => ArrayRuntime.cloneArray(x)
    case x: Array[Byte]    => ArrayRuntime.cloneArray(x)
    case x: Array[Short]   => ArrayRuntime.cloneArray(x)
    case x: Array[Boolean] => ArrayRuntime.cloneArray(x)
    case x: Array[Unit]    => x
    case null => throw new NullPointerException
  }

  /** Convert a numeric value array to an object array.
   *  Needed to deal with vararg arguments of primitive types that are passed
   *  to a generic Java vararg parameter T ...
   */
  def toObjectArray(src: AnyRef): Array[Object] = {
    val length = array_length(src)
    val dest = new Array[Object](length)
    for (i <- 0 until length)
      array_update(dest, i, array_apply(src, i))
    dest
  }

  def toArray[T](xs: collection.Seq[T]) = {
    val arr = new Array[AnyRef](xs.length)
    var i = 0
    for (x <- xs) {
      arr(i) = x.asInstanceOf[AnyRef]
      i += 1
    }
    arr
  }
  
  // Java bug: http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4071957
  // More background at ticket #2318.
  def ensureAccessible(m: JMethod): JMethod = {
    if (!m.isAccessible) {
      try m setAccessible true
      catch { case _: SecurityException => () }
    }
    m    
  }

  def checkInitialized[T <: AnyRef](x: T): T = 
    if (x == null) throw new UninitializedError else x

  abstract class Try[+A] {
    def Catch[B >: A](handler: PartialFunction[Throwable, B]): B
    def Finally(fin: => Unit): A
  }

  def Try[A](block: => A): Try[A] = new Try[A] with Runnable {
    private var result: A = _
    private var exception: Throwable = 
      try   { run() ; null }
      catch { 
        case e: ControlThrowable  => throw e  // don't catch non-local returns etc
        case e: Throwable         => e
      }

    def run() { result = block }

    def Catch[B >: A](handler: PartialFunction[Throwable, B]): B =
      if (exception == null) result
      else if (handler isDefinedAt exception) handler(exception)
      else throw exception

    def Finally(fin: => Unit): A = {
      fin

      if (exception == null) result
      else throw exception
    }
  }

  def _toString(x: Product): String =
    x.productIterator.mkString(x.productPrefix + "(", ",", ")")
  
  def _hashCode(x: Product): Int = {
    import scala.util.MurmurHash._
    val arr =  x.productArity
    var h = startHash(arr)
    var c = startMagicA
    var k = startMagicB
    var i = 0
    while (i < arr) {
      val elem = x.productElement(i)
      h = extendHash(h, if (elem == null) 0 else elem.##, c, k)
      c = nextMagicA(c)
      k = nextMagicB(k)
      i += 1
    }
    finalizeHash(h)
  }

  /** Fast path equality method for inlining; used when -optimise is set.
   */
  @inline def inlinedEquals(x: Object, y: Object): Boolean = 
    if (x eq y) true
    else if (x eq null) false
    else if (x.isInstanceOf[java.lang.Number]) BoxesRunTime.equalsNumObject(x.asInstanceOf[java.lang.Number], y)
    else if (x.isInstanceOf[java.lang.Character]) BoxesRunTime.equalsCharObject(x.asInstanceOf[java.lang.Character], y)
    else x.equals(y)

  def _equals(x: Product, y: Any): Boolean = y match {
    case y: Product if x.productArity == y.productArity => x.productIterator sameElements y.productIterator
    case _                                              => false
  }
  
  // hashcode -----------------------------------------------------------
  //
  // Note that these are the implementations called by ##, so they
  // must not call ## themselves.
 
  @inline def hash(x: Any): Int =
    if (x.isInstanceOf[java.lang.Number]) BoxesRunTime.hashFromNumber(x.asInstanceOf[java.lang.Number])
    else x.hashCode
  
  @inline def hash(dv: Double): Int = {
    val iv = dv.toInt
    if (iv == dv) return iv
    
    val lv = dv.toLong
    if (lv == dv) return lv.hashCode

    val fv = dv.toFloat
    if (fv == dv) fv.hashCode else dv.hashCode
  }
  @inline def hash(fv: Float): Int = {
    val iv = fv.toInt
    if (iv == fv) return iv
    
    val lv = fv.toLong
    if (lv == fv) return lv.hashCode
    else fv.hashCode
  }
  @inline def hash(lv: Long): Int = {
    val iv = lv.toInt
    if (iv == lv) iv else lv.hashCode
  }
  @inline def hash(x: Int): Int = x
  @inline def hash(x: Short): Int = x.toInt
  @inline def hash(x: Byte): Int = x.toInt
  @inline def hash(x: Char): Int = x.toInt
  @inline def hash(x: Boolean): Int = x.hashCode
  @inline def hash(x: Unit): Int = 0
  
  @inline def hash(x: Number): Int  = runtime.BoxesRunTime.hashFromNumber(x)
  
  /** XXX Why is there one boxed implementation in here? It would seem
   *  we should have all the numbers or none of them.
   */
  @inline def hash(x: java.lang.Long): Int = {
    val iv = x.intValue
    if (iv == x.longValue) iv else x.hashCode
  }

  /** A helper method for constructing case class equality methods,
   *  because existential types get in the way of a clean outcome and
   *  it's performing a series of Any/Any equals comparisons anyway.
   *  See ticket #2867 for specifics.
   */
  def sameElements(xs1: collection.Seq[Any], xs2: collection.Seq[Any]) = xs1 sameElements xs2

  /** Given any Scala value, convert it to a String.
   *
   * The primary motivation for this method is to provide a means for
   * correctly obtaining a String representation of a value, while
   * avoiding the pitfalls of naïvely calling toString on said value.
   * In particular, it addresses the fact that (a) toString cannot be
   * called on null and (b) depending on the apparent type of an
   * array, toString may or may not print it in a human-readable form.
   *
   * @param arg the value to stringify 
   * @return a string representation of <code>arg</code>
   *
   */  
  def stringOf(arg: Any): String = stringOf(arg, scala.Int.MaxValue)
  def stringOf(arg: Any, maxElements: Int): String = {    
    def isScalaClass(x: AnyRef) =
      Option(x.getClass.getPackage) exists (_.getName startsWith "scala.")
    
    def isTuple(x: AnyRef) =
      x.getClass.getName matches """^scala\.Tuple(\d+).*"""

    // When doing our own iteration is dangerous
    def useOwnToString(x: Any) = x match {
      // Node extends NodeSeq extends Seq[Node] and MetaData extends Iterable[MetaData]
      case _: Node | _: MetaData => true
      // Range/NumericRange have a custom toString to avoid walking a gazillion elements
      case _: Range | _: NumericRange[_] => true
      // Sorted collections to the wrong thing (for us) on iteration - ticket #3493
      case _: Sorted[_, _]  => true
      // StringBuilder(a, b, c) is not so attractive
      case _: StringBuilder => true
      // Don't want to evaluate any elements in a view
      case _: TraversableView[_, _] => true
      // Don't want to a) traverse infinity or b) be overly helpful with peoples' custom
      // collections which may have useful toString methods - ticket #3710
      case x: Traversable[_]  => !x.hasDefiniteSize || !isScalaClass(x)
      // Otherwise, nothing could possibly go wrong
      case _ => false
    }

    // A variation on inner for maps so they print -> instead of bare tuples
    def mapInner(arg: Any): String = arg match {
      case (k, v)   => inner(k) + " -> " + inner(v)
      case _        => inner(arg)
    }
    // The recursively applied attempt to prettify Array printing
    def inner(arg: Any): String = arg match {
      case null                         => "null"
      case ""                           => "\"\""
      case x: String                    => if (x.head.isWhitespace || x.last.isWhitespace) "\"" + x + "\"" else x
      case x if useOwnToString(x)       => x.toString
      case x: AnyRef if isArray(x)      => WrappedArray make x take maxElements map inner mkString ("Array(", ", ", ")")
      case x: collection.Map[_, _]      => x take maxElements map mapInner mkString (x.stringPrefix + "(", ", ", ")")
      case x: Traversable[_]            => x take maxElements map inner mkString (x.stringPrefix + "(", ", ", ")")
      case x: Product1[_] if isTuple(x) => "(" + inner(x._1) + ",)" // that special trailing comma
      case x: Product if isTuple(x)     => x.productIterator map inner mkString ("(", ",", ")")
      case x                            => x toString
    }

    // The try/catch is defense against iterables which aren't actually designed
    // to be iterated, such as some scala.tools.nsc.io.AbstractFile derived classes.
    val s = 
      try inner(arg)
      catch { 
        case _: StackOverflowError | _: UnsupportedOperationException => arg.toString
      }
        
    val nl = if (s contains "\n") "\n" else ""
    nl + s + "\n"    
  }
}
