//> using options -Wunused:params -Werror
//

import Answers._

trait InterFace {
  /** Call something. */
  def call(a: Int, b: String, c: Double): Int
}

trait BadAPI extends InterFace {
  def f(a: Int,
        b: String,               // warn
        c: Double): Int = {
    println(c)
    a
  }
  @deprecated("no warn in deprecated API", since="yesterday")
  def g(a: Int,
        b: String,               // no warn
        c: Double): Int = {
    println(c)
    a
  }
  override def call(a: Int,
                    b: String,               // no warn, required by superclass
                    c: Double): Int = {
    println(c)
    a
  }

  def meth(x: Int) = x

  override def equals(other: Any): Boolean = true  // no warn

  def i(implicit s: String) = answer           // yes, warn

  /*
  def future(x: Int): Int = {
    val y = 42
    val x = y               // maybe option to warn only if shadowed
    x
  }
  */
}

// mustn't alter warnings in super
trait PoorClient extends BadAPI {
  override def meth(x: Int) = ???       // no warn
  override def f(a: Int, b: String, c: Double): Int = a + b.toInt + c.toInt
}

class Unusing(u: Int) {       // warn
  def f = ???
}

class Valuing(val u: Int)        // no warn

class Revaluing(u: Int) { def f = u } // no warn

case class CaseyKasem(k: Int)        // no warn

case class CaseyAtTheBat(k: Int)(s: String)        // warn

trait Ignorance {
  def f(readResolve: Int) = answer           // warn
}

class Reusing(u: Int) extends Unusing(u)   // no warn

class Main {
  def main(args: Array[String]): Unit = println("hello, args")  // no warn
}

trait Unimplementation {
  def f(u: Int): Int = ???        // no warn for param in unimplementation
}

trait DumbStuff {
  def f(implicit dummy: DummyImplicit) = answer
  def g(dummy: DummyImplicit) = answer
}
trait Proofs {
  def f[A, B](implicit ev: A =:= B) = answer
  def g[A, B](implicit ev: A <:< B) = answer
  def f2[A, B](ev: A =:= B) = answer
  def g2[A, B](ev: A <:< B) = answer
}

trait Anonymous {
  def f = (i: Int) => answer      // warn

  def f1 = (_: Int) => answer     // no warn underscore parameter (a fresh name)

  def f2: Int => Int = _ + 1  // no warn placeholder syntax (a fresh name and synthetic parameter)

  def g = for (i <- List(1)) yield answer    // warn map.(i => 42)
}
trait Context[A] { def m(a: A): A = a }
trait Implicits {
  def f[A](implicit ctx: Context[A]) = answer
  def g[A: Context] = answer
}
class Bound[A: Context]
object Answers {
  def answer: Int = 42
}

trait BadMix { _: InterFace =>
  def f(a: Int,
        b: String,               // warn
        c: Double): Int = {
    println(c)
    a
  }
  @deprecated("no warn in deprecated API", since="yesterday")
  def g(a: Int,
        b: String,               // no warn
        c: Double): Int = {
    println(c)
    a
  }
  override def call(a: Int,
                    b: String,               // no warn, required by superclass
                    c: Double): Int = {
    println(c)
    a
  }

  def meth(x: Int) = x

  override def equals(other: Any): Boolean = true  // no warn

  def i(implicit s: String) = answer           // yes, warn
}

class Unequal {
  override def equals(other: Any) = toString.nonEmpty   // no warn non-trivial RHS, required by universal method
}

class Seriously {
  def f(s: Serializable) = toString.nonEmpty  // warn explicit param of marker trait
}

class TryStart(start: String) {
  def FINALLY(end: END.type) = start
}

object END

class Nested {
  @annotation.unused private def actuallyNotUsed(fresh: Int, stale: Int) = fresh
}
