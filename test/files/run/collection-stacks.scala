import scala.collection.mutable

object Test extends App {
  def mutableStack[T](xs: T*): mutable.Stack[T] = {
    val s = new mutable.Stack[T]
    s.pushAll(xs)
    s
  }

  def check[T](expected: T, got: T): Unit = {
    println(s"$got: ${expected == got}")
  }

  // check #957
  check("3-2-1", mutableStack(1, 2, 3).iterator.mkString("-"))

  println("apply")
  check(3, mutableStack(1, 2, 3).apply(0))
  check(1, mutableStack(1, 2, 3).apply(2))

  println("top")
  check(3, mutableStack(1, 2, 3).top)

  println("pop")
  check(3, mutableStack(1, 2, 3).pop())
  check("2-1", { val s = mutableStack(1, 2, 3); s.pop(); s.toList.mkString("-") })
}
