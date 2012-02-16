/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

/** This program generates the ProductN, TupleN, FunctionN,
 *  and AbstractFunctionN, where 0 <= N <= MAX_ARITY.
 *
 *    Usage: scala genprod <directory>
 *      where the argument is the desired output directory
 *
 *  @author  Burak Emir, Stephane Micheloud, Geoffrey Washburn, Paul Phillips
 *  @version 1.1
 */
object genprod extends App {
  val MAX_ARITY = 22
  def arities = (1 to MAX_ARITY).toList

  class Group(val name: String) {
    def className(i: Int) = name + i
    def fileName(i: Int) = className(i) + ".scala"
  }

  def productFiles  = arities map Product.make
  def tupleFiles    = arities map Tuple.make
  def functionFiles = (0 :: arities) map Function.make
  def absFunctionFiles = (0 :: arities) map AbstractFunction.make
  def allfiles      = productFiles ::: tupleFiles ::: functionFiles ::: absFunctionFiles

  trait Arity extends Group {
    def i: Int    // arity

    def typeArgsString(xs: Seq[String]) = xs.mkString("[", ", ", "]")

    def to              = (1 to i).toList
    def s               = if (i == 1) "" else "s"
    def className       = name + i
    def classAnnotation = ""
    def fileName        = className + ".scala"
    def targs           = to map ("T" + _)
    def vdefs           = to map ("v" + _)
    def xdefs           = to map ("x" + _)
    def mdefs           = to map ("_" + _)
    def invariantArgs   = typeArgsString(targs)
    def covariantArgs   = typeArgsString(targs map (covariantSpecs + "+" + _))
    def covariantSpecs  = ""
    def contravariantSpecs = ""
    def contraCoArgs    = typeArgsString((targs map (contravariantSpecs + "-" + _)) ::: List(covariantSpecs + "+R"))
    def constructorArgs = (targs).map( _.toLowerCase ) mkString ", "
    def fields          = (mdefs, targs).zipped.map(_ + ": " + _) mkString ", "
    def funArgs         = (vdefs, targs).zipped.map(_ + ": " + _) mkString ", "

    def genprodString       = " See scala.Function0 for timestamp."
    def moreMethods         = ""
    def packageDef          = "scala"
    def imports             = ""

    def header = """
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT.%s

package %s
%s
""".trim.format(genprodString, packageDef, imports)
  }

  if (args.length != 1) {
    println("please give path of output directory")
    exit(-1)
  }
  val out = args(0)
  def writeFile(node: scala.xml.Node) {
    import scala.tools.nsc.io._
    val f = Path(out) / node.attributes("name").toString
    f.parent.createDirectory(force = true)
    f.toFile writeAll node.text
  }

  allfiles foreach writeFile
}
import genprod._


/* zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
                             F U N C T I O N
zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz */

object FunctionZero extends Function(0) {
  override def genprodString  = "\n// genprod generated these sources at: " + new java.util.Date()
  override def covariantSpecs = "@specialized "
  override def descriptiveComment = "  " + functionNTemplate.format("javaVersion", "anonfun0",
"""
 *    val javaVersion = () => sys.props("java.version")
 *
 *    val anonfun0 = new Function0[String] {
 *      def apply(): String = sys.props("java.version")
 *    }
 *    assert(javaVersion() == anonfun0())
 * """)
  override def moreMethods = ""
}

object FunctionOne extends Function(1) {
  override def classAnnotation    = "@annotation.implicitNotFound(msg = \"No implicit view available from ${T1} => ${R}.\")\n"
  override def contravariantSpecs = "@specialized(scala.Int, scala.Long, scala.Float, scala.Double, scala.AnyRef) "
  override def covariantSpecs     = "@specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double, scala.AnyRef) "

  override def descriptiveComment = "  " + functionNTemplate.format("succ", "anonfun1",
"""
 *    val succ = (x: Int) => x + 1
 *    val anonfun1 = new Function1[Int, Int] {
 *      def apply(x: Int): Int = x + 1
 *    }
 *    assert(succ(0) == anonfun1(0))
 * """)

  override def moreMethods = """
  /** Composes two instances of Function1 in a new Function1, with this function applied last.
   *
   *  @tparam   A   the type to which function `g` can be applied
   *  @param    g   a function A => T1
   *  @return       a new function `f` such that `f(x) == apply(g(x))`
   */
  def compose[A](g: A => T1): A => R = { x => apply(g(x)) }

  /** Composes two instances of Function1 in a new Function1, with this function applied first.
   *
   *  @tparam   A   the result type of function `g`
   *  @param    g   a function R => A
   *  @return       a new function `f` such that `f(x) == g(apply(x))`
   */
  def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }
"""
}

object FunctionTwo extends Function(2) {
  override def contravariantSpecs = "@specialized(scala.Int, scala.Long, scala.Double) "
  override def covariantSpecs = "@specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) "

  override def descriptiveComment = "  " + functionNTemplate.format("max", "anonfun2",
"""
 *    val max = (x: Int, y: Int) => if (x < y) y else x
 *
 *    val anonfun2 = new Function2[Int, Int, Int] {
 *      def apply(x: Int, y: Int): Int = if (x < y) y else x
 *    }
 *    assert(max(0, 1) == anonfun2(0, 1))
 * """)
}

object Function {
  def make(i: Int) = apply(i)()
  def apply(i: Int) = i match {
    case 0    => FunctionZero
    case 1    => FunctionOne
    case 2    => FunctionTwo
    case _    => new Function(i)
  }
}

class Function(val i: Int) extends Group("Function") with Arity {
  def descriptiveComment  = ""
  def functionNTemplate = """
 *  In the following example, the definition of %s is a
 *  shorthand for the anonymous class definition %s:
 *
 *  {{{
 *  object Main extends App { %s }
 *  }}}
 *
 *  Note that `Function1` does not define a total function, as might
 *  be suggested by the existence of [[scala.PartialFunction]]. The only
 *  distinction between `Function1` and `PartialFunction` is that the
 *  latter can specify inputs which it will not handle.
 """

  def toStr() = "\"" + ("<function%d>" format i) + "\""
  def apply() = {
<file name={fileName}>{header}

/** A function of {i} parameter{s}.
 *{descriptiveComment}
 */
{classAnnotation}trait {className}{contraCoArgs} extends AnyRef {{ self =>
  /** Apply the body of this function to the argument{s}.
   *  @return   the result of function application.
   */
  def apply({funArgs}): R
  {moreMethods}
  override def toString() = {toStr}
}}
</file>
}

  private def commaXs = xdefs.mkString("(", ", ", ")")

  // (x1: T1) => (x2: T2) => (x3: T3) => (x4: T4) => apply(x1,x2,x3,x4)
  def shortCurry = {
    val body = "apply" + commaXs
    (xdefs, targs).zipped.map("(%s: %s) => ".format(_, _)).mkString("", "", body)
  }

  // (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7) => self.apply(x1,x2,x3,x4,x5,x6,x7)).curried
  def longCurry = ((xdefs, targs).zipped.map(_ + ": " + _) drop 1).mkString(
    "(x1: T1) => ((",
    ", ",
    ") => self.apply%s).curried".format(commaXs)
  )

  // f(x1,x2,x3,x4,x5,x6)  == (f.curried)(x1)(x2)(x3)(x4)(x5)(x6)
  def curryComment = {
"""/** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f%s == apply%s`
   */""".format(xdefs map ("(" + _ + ")") mkString, commaXs)
  }

  def tupleMethod = {
    def comment = """
  /** Creates a tupled version of this function: instead of %d arguments,
   *  it accepts a single [[scala.Tuple%d]] argument.
   *
   *  @return   a function `f` such that `f(%s) == f(Tuple%d%s) == apply%s`
   */
""".format(i, i, commaXs, i, commaXs, commaXs)
    def body = "case Tuple%d%s => apply%s".format(i, commaXs, commaXs)

    comment + "  def tupled: Tuple%d%s => R = {\n    %s\n  }".format(i, invariantArgs, body)
  }

  def curryMethod = {
    val body = if (i < 5) shortCurry else longCurry

    curryComment +
    "  def curried: %s => R = {\n    %s\n  }\n".format(
      targs mkString " => ", body
    )
  }

  override def moreMethods = curryMethod + tupleMethod
} // object Function


/* zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
                                     T U P L E
zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz */

object Tuple {
  val zipImports = """
import scala.collection.{ TraversableLike => TLike, IterableLike => ILike }
import scala.collection.generic.{ CanBuildFrom => CBF }
"""

  def make(i: Int) = apply(i)()
  def apply(i: Int) = i match {
    case 1  => TupleOne
    case 2  => TupleTwo
    case 3  => TupleThree
    case _  => new Tuple(i)
  }
}

object TupleOne extends Tuple(1)
{
  override def covariantSpecs = "@specialized(Int, Long, Double) "
}

object TupleTwo extends Tuple(2)
{
  override def imports = Tuple.zipImports
  override def covariantSpecs = "@specialized(Int, Long, Double) "
  override def moreMethods = """
  /** Swaps the elements of this `Tuple`.
   * @return a new Tuple where the first element is the second element of this Tuple and the
   * second element is the first element of this Tuple.
   */
  def swap: Tuple2[T2,T1] = Tuple2(_2, _1)

  @deprecated("Use `zipped` instead.", "2.9.0")
  def zip[Repr1, El1, El2, To](implicit w1:   T1 => TLike[El1, Repr1],
                                        w2:   T2 => Iterable[El2],
                                        cbf1: CBF[Repr1, (El1, El2), To]): To = {
    zipped map ((x, y) => ((x, y)))
  }

  /** Wraps a tuple in a `Zipped`, which supports 2-ary generalisations of `map`, `flatMap`, `filter`, etc.
   * Note that there must be an implicit value to convert this tuple's types into a [[scala.collection.TraversableLike]]
   * or [[scala.collection.IterableLike]].
   * {{{
   * scala> val tuple = (List(1,2,3),List('a','b','c'))
   * tuple: (List[Int], List[Char]) = (List(1, 2, 3),List(a, b, c))
   *
   * scala> tuple.zipped map { (x,y) => x + ":" + y }
   * res6: List[java.lang.String] = List(1:a, 2:b, 3:c)
   * }}}
   *
   * @see Zipped
   * Note: will not terminate for infinite-sized collections.
   */
  def zipped[Repr1, El1, Repr2, El2](implicit w1: T1 => TLike[El1, Repr1], w2: T2 => ILike[El2, Repr2]): Zipped[Repr1, El1, Repr2, El2]
    = new Zipped[Repr1, El1, Repr2, El2](_1, _2)

  class Zipped[+Repr1, +El1, +Repr2, +El2](coll1: TLike[El1, Repr1], coll2: ILike[El2, Repr2]) { // coll2: ILike for filter
    def map[B, To](f: (El1, El2) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      b.sizeHint(coll1)
      val elems2 = coll2.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext)
          b += f(el1, elems2.next)
        else
          return b.result
      }

      b.result
    }

    def flatMap[B, To](f: (El1, El2) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext)
          b ++= f(el1, elems2.next)
        else
          return b.result
      }

      b.result
    }

    def filter[To1, To2](f: (El1, El2) => Boolean)(implicit cbf1: CBF[Repr1, El1, To1], cbf2: CBF[Repr2, El2, To2]): (To1, To2) = {
      val b1 = cbf1(coll1.repr)
      val b2 = cbf2(coll2.repr)
      val elems2 = coll2.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext) {
          val el2 = elems2.next
          if (f(el1, el2)) {
            b1 += el1
            b2 += el2
          }
        }
        else return (b1.result, b2.result)
      }

      (b1.result, b2.result)
    }

    def exists(f: (El1, El2) => Boolean): Boolean = {
      val elems2 = coll2.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext) {
          if (f(el1, elems2.next))
            return true
        }
        else return false
      }
      false
    }

    def forall(f: (El1, El2) => Boolean): Boolean =
      !exists((x, y) => !f(x, y))

    def foreach[U](f: (El1, El2) => U): Unit = {
      val elems2 = coll2.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext)
          f(el1, elems2.next)
        else
          return
      }
    }
  }
"""
}

object TupleThree extends Tuple(3) {
  override def imports = Tuple.zipImports
  override def moreMethods = """

  @deprecated("Use `zipped` instead.", "2.9.0")
  def zip[Repr1, El1, El2, El3, To](implicit w1:   T1 => TLike[El1, Repr1],
                                             w2:   T2 => Iterable[El2],
                                             w3:   T3 => Iterable[El3],
                                             cbf1: CBF[Repr1, (El1, El2, El3), To]): To = {
    zipped map ((x, y, z) => ((x, y, z)))
  }

  /** Wraps a tuple in a `Zipped`, which supports 3-ary generalisations of `map`, `flatMap`, `filter`, etc.
   * Note that there must be an implicit value to convert this tuple's types into a [[scala.collection.TraversableLike]]
   * or [[scala.collection.IterableLike]].
   * {{{
   * scala> val tuple = (List(1,2,3),List('a','b','c'),List("x","y","z"))
   * tuple: (List[Int], List[Char], List[java.lang.String]) = (List(1, 2, 3),List(a, b, c),List(x, y, z))
   *
   * scala> tuple.zipped map { (x,y,z) => x + ":" + y + ":" + z}
   * res8: List[java.lang.String] = List(1:a:x, 2:b:y, 3:c:z)
   * }}}
   *
   * @see Zipped
   * Note: will not terminate for infinite-sized collections.
   */
  def zipped[Repr1, El1, Repr2, El2, Repr3, El3](implicit w1: T1 => TLike[El1, Repr1],
                                                          w2: T2 => ILike[El2, Repr2],
                                                          w3: T3 => ILike[El3, Repr3]): Zipped[Repr1, El1, Repr2, El2, Repr3, El3]
    = new Zipped[Repr1, El1, Repr2, El2, Repr3, El3](_1, _2, _3)

  class Zipped[+Repr1, +El1, +Repr2, +El2, +Repr3, +El3](coll1: TLike[El1, Repr1],
                                                         coll2: ILike[El2, Repr2],
                                                         coll3: ILike[El3, Repr3]) {
    def map[B, To](f: (El1, El2, El3) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          b += f(el1, elems2.next, elems3.next)
        else
          return b.result
      }
      b.result
    }

    def flatMap[B, To](f: (El1, El2, El3) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          b ++= f(el1, elems2.next, elems3.next)
        else
          return b.result
      }
      b.result
    }

    def filter[To1, To2, To3](f: (El1, El2, El3) => Boolean)(
                 implicit cbf1: CBF[Repr1, El1, To1],
                          cbf2: CBF[Repr2, El2, To2],
                          cbf3: CBF[Repr3, El3, To3]): (To1, To2, To3) = {
      val b1 = cbf1(coll1.repr)
      val b2 = cbf2(coll2.repr)
      val b3 = cbf3(coll3.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator
      def result = (b1.result, b2.result, b3.result)

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext) {
          val el2 = elems2.next
          val el3 = elems3.next

          if (f(el1, el2, el3)) {
            b1 += el1
            b2 += el2
            b3 += el3
          }
        }
        else return result
      }

      result
    }

    def exists(f: (El1, El2, El3) => Boolean): Boolean = {
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext) {
          if (f(el1, elems2.next, elems3.next))
            return true
        }
        else return false
      }
      false
    }

    def forall(f: (El1, El2, El3) => Boolean): Boolean =
      !exists((x, y, z) => !f(x, y, z))

    def foreach[U](f: (El1, El2, El3) => U): Unit = {
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          f(el1, elems2.next, elems3.next)
        else
          return
      }
    }
  }
"""
}

class Tuple(val i: Int) extends Group("Tuple") with Arity {
  private def idiomatic =
    if (i < 2) ""
    else " Note that it is more idiomatic to create a %s via `(%s)`".format(className, constructorArgs)

  private def params = (
    1 to i map (x => " *  @param  _%d   Element %d of this Tuple%d".format(x, x, i))
  ) mkString "\n"

  // prettifies it a little if it's overlong
  def mkToString() = {
  def str(xs: List[String]) = xs.mkString(""" + "," + """)
    if (i <= MAX_ARITY / 2) str(mdefs)
    else {
      val s1 = str(mdefs take (i / 2))
      val s2 = str(mdefs drop (i / 2))
      s1 + " +\n    \",\" + " + s2
    }
  }

  def apply() = {
<file name={fileName}>{header}

/** A tuple of {i} elements; the canonical representation of a [[scala.{Product.className(i)}]].
 *
 *  @constructor  Create a new tuple with {i} elements.{idiomatic}
{params}
 */
case class {className}{covariantArgs}({fields})
  extends {Product.className(i)}{invariantArgs}
{{
  override def toString() = "(" + {mkToString} + ")"
  {moreMethods}
}}
</file>}
} // object Tuple


/* zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
                                  P R O D U C T
zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz */

object Product extends Group("Product")
{
  def make(i: Int) = apply(i)()
  def apply(i: Int) = i match {
    case 1  => ProductOne
    case 2  => ProductTwo
    case _ => new Product(i)
  }
}

object ProductOne extends Product(1)
{
  override def covariantSpecs = "@specialized(Int, Long, Double) "
}

object ProductTwo extends Product(2)
{
  override def covariantSpecs = "@specialized(Int, Long, Double) "
}

class Product(val i: Int) extends Group("Product") with Arity {
  val productElementComment = """
  /** Returns the n-th projection of this product if 0 < n <= productArity,
   *  otherwise throws an `IndexOutOfBoundsException`.
   *
   *  @param n number of the projection to be returned
   *  @return  same as `._(n+1)`, for example `productElement(1)` is the same as `._1`.
   *  @throws  IndexOutOfBoundsException
   */
"""

  def cases = {
    val xs = for ((x, i) <- mdefs.zipWithIndex) yield "case %d => %s".format(i, x)
    val default = "case _ => throw new IndexOutOfBoundsException(n.toString())"
    "\n" + ((xs ::: List(default)) map ("    " + _ + "\n") mkString)
  }
  def proj = {
    (mdefs,targs).zipped.map( (_,_) ).zipWithIndex.map { case ((method,typeName),index) =>
      """|  /** A projection of element %d of this Product.
         |   *  @return   A projection of element %d.
         |   */
         |  def %s: %s
         |""".stripMargin.format(index + 1, index + 1, method, typeName)
    } mkString
  }

  def apply() = {
<file name={fileName}>{header}
object {className} {{
  def unapply{invariantArgs}(x: {className}{invariantArgs}): Option[{className}{invariantArgs}] =
    Some(x)
}}

/** {className} is a cartesian product of {i} component{s}.
 *  @since 2.3
 */
trait {className}{covariantArgs} extends Product {{
  /** The arity of this product.
   *  @return {i}
   */
  override def productArity = {i}

  {productElementComment}
  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int) = n match {{ {cases} }}

{proj}
{moreMethods}
}}
</file>}

}

/** Abstract functions **/

object AbstractFunctionZero extends AbstractFunction(0) {
  override def covariantSpecs = FunctionZero.covariantSpecs
}

object AbstractFunctionOne extends AbstractFunction(1) {
  override def covariantSpecs = FunctionOne.covariantSpecs
  override def contravariantSpecs = FunctionOne.contravariantSpecs
}

object AbstractFunctionTwo extends AbstractFunction(2) {
  override def covariantSpecs = FunctionTwo.covariantSpecs
  override def contravariantSpecs = FunctionTwo.contravariantSpecs
}

class AbstractFunction(val i: Int) extends Group("AbstractFunction") with Arity
{
  override def packageDef = "scala.runtime"

  val superTypeArgs = typeArgsString(targs ::: List("R"))

  def apply() = {
<file name={"runtime/" + fileName}>{header}
abstract class {className}{contraCoArgs} extends Function{i}{superTypeArgs} {{
{moreMethods}
}}
</file>}

}
object AbstractFunction
{
  def make(i: Int) = apply(i)()
  def apply(i: Int) = i match {
    case 0    => AbstractFunctionZero
    case 1    => AbstractFunctionOne
    case 2    => AbstractFunctionTwo
    case _    => new AbstractFunction(i)
  }
}
