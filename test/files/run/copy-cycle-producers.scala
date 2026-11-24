//> using options -opt:local

import scala.reflect.ClassTag

// The variables in these methods go through a cycle of copies. ProdConsAnalyzer used to cache the
// answers it had halfway around the cycle, so `-opt:local` saw only some of the values that a
// variable can hold at its last use.
object Box {
  var escaped: Integer = null

  // Box-unbox used to replace the box with an `int` local that is not assigned on every path,
  // so that the method failed verification.
  def f(p: Integer, b: Boolean, c: Boolean, n0: Int): Int = {
    var n = n0
    var a: Integer = p
    var r = 0
    do {
      a = if (b) Integer.valueOf(84) else a
      r += a.intValue
      a = if (c) a else Integer.valueOf(15)
      if (c) escaped = a
      n -= 1
    } while (n > 0)
    r + a.intValue
  }
}

object Test {
  // The array type used to be taken from the `ClassTag` in the loop.
  def newArray(p: ClassTag[AnyRef], b: Boolean, c: Boolean): AnyRef = {
    var a: ClassTag[AnyRef] = p
    do {
      a = if (b) a else ClassTag[AnyRef](classOf[String])
    } while (c)
    a.newArray(1)
  }

  // The last invocation used to be rewritten to a call of the lambda body `x + 2`.
  def closure(p: Int => Int, b: Boolean, c: Boolean, d: Boolean): Int = {
    var f: Int => Int = p
    var r = 0
    do {
      f = if (b) f else ((x: Int) => x + 1)
      r += f(1)
      f = if (c) f else ((x: Int) => x + 2)
    } while (d)
    r + f(10)
  }

  def main(args: Array[String]): Unit = {
    println(newArray(ClassTag[AnyRef](classOf[Integer]), b = true, c = false).getClass)
    println(closure(_ * 5, b = true, c = true, d = false))
    println(closure(_ * 5, b = false, c = true, d = false))
    println(Box.f(Integer.valueOf(7), b = false, c = true, n0 = 2))
    println(Box.escaped)
  }
}
