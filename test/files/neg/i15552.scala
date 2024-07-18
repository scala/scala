//> using options -Werror
class C {
  val ctx: C = new C()
  def f() = g(42)(using ctx)  // error: no g
}

class Innocent {
  val using = 42
  def g(i: Int) = i
  def f() = g(using)
}

class Malicious {
  val using = 42
  def g(i: Int) = i
  def f() = g(using using)
}

class Suggest {
  import scala.util._
  def g(i: Int) = i
  def f() = g(using)          // error: no using, does not actually suggest Using
}

class Fancy {
  def g(i: Int) = i
  def f() = g(using if (true) 42 else 17)
}

/*
was:

i15552.scala:4: error: not found: value g
  def f() = g(42)(using ctx)
            ^
i15552.scala:4: error: not found: value using
  def f() = g(42)(using ctx)
                  ^
i15552.scala:4: error: postfix operator ctx needs to be enabled
 */
