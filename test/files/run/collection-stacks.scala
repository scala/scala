import scala.collection.{ immutable, mutable }

object Test extends App {
  def mutableStack[T](xs: T*): mutable.Stack[T] = {
    val s = new mutable.Stack[T]
    s.pushAll(xs)
    s
  }

  def immutableStack[T](xs: T*): immutable.Stack[T] = {
    immutable.Stack.empty[T] pushAll xs
  }

  def check[T](expected: T, got: T) {
    println(got + ": " + (expected == got))
  }

  // check #957
  check("3-2-1", immutableStack(1, 2, 3).iterator.mkString("-"))
  check("3-2-1", mutableStack(1, 2, 3).iterator.mkString("-"))

  println("apply")
  check(3, immutableStack(1, 2, 3).apply(0))
  check(3, mutableStack(1, 2, 3).apply(0))
  check(1, immutableStack(1, 2, 3).apply(2))
  check(1, mutableStack(1, 2, 3).apply(2))

  println("top")
  check(3, immutableStack(1, 2, 3).top)
  check(3, mutableStack(1, 2, 3).top)

  println("pop")
  check("2-1", immutableStack(1, 2, 3).pop.mkString("-"))
  check(3, mutableStack(1, 2, 3).pop())
  check("2-1", { val s = mutableStack(1, 2, 3); s.pop(); s.toList.mkString("-") })
}

// vim: set ts=2 sw=2 et:
