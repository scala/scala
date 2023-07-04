// scalac: -Xmigration -Xsource:3

// Demonstrate migration warnings at typer for -Xsource:3

class `named arg is not assignment` {
  // unfortunately, z member is not available yet while erroring in g
  //var z = 17
  def f(x: Int, y: Int) = x + y
  def g = {
    var z = 17
    f(42, z = 27)
  }
}

class `interpolated unicode such as \u0043` {
  def entry = "Cat"
  def f = raw"\u0043 is for $entry"
  def g = raw"""\u0043 is for Cat"""
}

// it was always specified that unary is parameterless.
// The most correct behavior would be that you can define unary_-()
// but you can't use it as unary prefix.
class `unary op lacks parens` {
  def unary_-() = -42
}

package object tester extends Runnable {
  def run() = ()
}

abstract class `procedure syntax` {
  def this(s: String) { this() }
  def f() { println() }
  def g()
}

class `lambda parens` {
  def f = List(42).map { x: Int => x + 1 }
}

class `infix type args` {
  def f = List(42) map [Int] (_ + 1)
}

class `misuse of underscore`[_]

class `early bird` extends { val x = "hello, world" } with Runnable { def run() = println(x) }

case class `case mods propagate` private (s: String)

case class `copyless case mods propagate` private (s: String) {
  def copy(x: String) = this
}

class Parent {
  def f: Option[Int] = Some(42)
}
class Child extends Parent {
  override def f = Some(27)
}

@annotation.nowarn
class `get off my back` {
  def f() { println("hello, world") }
}
