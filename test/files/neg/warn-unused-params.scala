// scalac: -Ywarn-unused:params -Xfatal-warnings

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
  @deprecated ("no warn in deprecated API", since="yesterday")
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

  def i(implicit s: String) = 42           // yes, warn

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
  def f(readResolve: Int) = 42           // warn
}

class Reusing(u: Int) extends Unusing(u)   // no warn

class Main {
  def main(args: Array[String]): Unit = println("hello, args")  // no warn
}

trait Unimplementation {
  def f(u: Int): Int = ???        // no warn for param in unimplementation
}

trait Anonymous {
  def f = (i: Int) => 42      // warn

  def f1 = (_: Int) => 42     // no warn underscore parameter (a fresh name)

  def f2: Int => Int = _ + 1  // no warn placeholder syntax (a fresh name and synthethic parameter)

  def g = for (i <- List(1)) yield 42    // warn map.(i => 42)
}
