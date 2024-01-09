//> using options -Werror -Xlint:pattern-shadow

trait T {
  val x = 42

  def f(i: Int) =
    i match {
      case `x` => 0     // presence of this case really obviates warning on the next?
      case x   => 1     // warn
    }
  def g(i: Int) =
    i match {
      case x @ _ => 1   // never warn if user writes bind of wildcard
    }
  def h(i: Any) =
    i match {
      case i: Int => i  // alias of scrutinee
      case _ => 42
    }
}
// same but in a class
class CT {
  val x = 42

  def f(i: Int) =
    i match {
      case `x` => 0     // presence of this case really obviates warning on the next?
      case x   => 1     // warn
    }
  def g(i: Int) =
    i match {
      case x @ _ => 1   // never warn if user writes bind of wildcard
    }
  def h(i: Any) =
    i match {
      case i: Int => i  // alias of scrutinee
      case _ => 42
    }
}
trait Overload[A] {
  def map[B](f: A => B): Int = ???
}
trait Overloader[K, V] extends Overload[(K, V)] {
  def map[K2, V2](f: ((K, V)) => (K2, V2)): String = ???

  def f() = this match {
    case map: Overloader[k, v] => // shadows overloaded members which are not stable
  }
}
class C {
  val (x, y) = (42, 27)
  def f(): Unit = {
    val (a, b) = (42, 27)
    println(a+b)
  }
}
final class D(private val xs: List[Int]) extends AnyVal {
  def f: List[Int] =
    (xs: Any @unchecked) match {
      case xs: List[_] => Nil
      case _ => Nil
    }
}
sealed class Tree
final case class Apply(fn: Tree, args: List[Tree]) extends Tree
final class Applied(val tree: Tree) {
  /** The tree stripped of the possibly nested applications.
   *  The original tree if it's not an application.
   */
  def callee: Tree = {
    @annotation.tailrec
    def loop(tree: Tree): Tree = tree match {
      case Apply(fn, _) => loop(fn)
      case tree         => tree   // alias of scrutinee
    }
    loop(tree)
  }

  def `ident introduced by case class`(): Unit = {
    val fn = 42
    tree match {
      case Apply(fn, Nil) => println(fn)  // name of parameter
      case _ => println(fn)
    }
  }

  def `ident introduced by case class with a twist`(): Unit = {
    val fn = 42
    tree match {
      case t @ Apply(fn, Nil) => println((t, fn)) // name of parameter but not top level pattern
      case _ => println(fn)
    }
  }

  def `bound var in pattern is selector`(t: Tree): Unit = {
    t match {
      case Apply(t, args) => println((t, args)) // alias of scrutinee but not top level pattern
      case _ =>
    }
  }
}
object X { def unapply(p: (Int, Int)): Option[(Int, Int)] = Option(p).filter(p => p._1 == p._2) }
object Y { def unapply(p: (Int, Int, Int)): Option[(Int, Int)] = Option(p).map { case (x, y, z) => (x, y+z) } }
class Tupling {
  def f(x: Int, y: Int): Int = (x, y) match {
    case (42, 27) => 5
    case (x, y)   => x+y // correspond to tuple arg
  }
  def g(x: Some[Int], y: Some[Int]): Int = (x, y) match {
    case (Some(42), Some(27)) => 5
    case (Some(x), Some(y))   => x+y // correspond to tuple arg but not top level pattern
  }
  def e(x: Int, y: Int): Int = (x, y) match {
    case X(x, y)  => x+y // extractor args correspond to tuple args
    case _        => -1
  }
  def err(x: Int, y: Int, z: Int): Int = (x, y, z) match {
    case Y(x, y)  => x+y // only allow 1-1
    case _        => -1
  }
}
class Selfie { self =>
  def f(x: Int, y: Selfie): Int = (x, y) match {
    case (42, this) => 5
    case (x, self) => x
    case _ => 42
  }
  def g(): Int = self match {
    case self: Selfie => 5
    case _ => 42
  }
}
class Deconstruct[K, +V](val mapping: Deconstruct.Mapping[K, V]) {
  def test(k: K): V = {
    val (_, v) = mapping(k)
    v
  }
}
object Deconstruct {
  type Mapping[K, +V] = Map[K, (Int, V)]
}
class Init {
  def f(): Int = 42
  val res = f() match {
    case res => res
  }
}
package p {
  class P {
    def m = ???
    def test(x: Any, y: => Any) = x match {
      case p: Int => p
      case m: String => m.toInt
      case y: Double => y.toInt
      case _ => 42
    }
  }
}
class `multi extraction of singular scrutinee` {
  val r = raw"(\d)(\d)".r
  val x = "42"
  def test = x match {
    case r(x, y) => x * y.toInt
    case _ => ""
  }
}
class `weird but true` {
  val _1 = "yup"
  def test = (42, 27) match {
    case (_1, 27) => 3 // briefly did not warn as param name
    case _ => 5
  }
}
case class Thing(i: Int, other: Thing)
class `derived thing is refinement` {
  val t0 = Thing(27, null)
  val t  = Thing(42, t0)
  t.other match {
    case t => // ok because select from t is another Thing maybe related
  }
  t.copy(i = 5) match {
    case t => // ok because deriving from t is another Thing maybe related
  }
}
class `kosher selector` {
  def f(x: Any) = 42 match {
    case x => x.toString
  }
}
class `lukas asked whats that null check for` {
  import annotation._
  def isOperatorPart(c: Char): Boolean = (c: @unchecked) match {
    case '+' => true
    case c => false
  }
}
case class Collector() {
  def collect[T](pf: PartialFunction[Collector, T]): List[T] = ???
  def flag = true
}
class `pattern matches occasionally appear in pattern-matching anonymous functions` {
  val c = Collector()
  def f = c.collect { case c if c.flag => c.toString }
}
