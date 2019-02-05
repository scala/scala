





trait A[@specialized(Int) T] {
  def foo(t: T): Unit
}


trait B extends A[Int] {
  def foo(t: Int): Unit = {
    println("B.foo")
  }
}


trait M extends B {
  abstract override def foo(t: Int): Unit = {
    super.foo(t)
    println("M.foo")
  }
}


object C extends B with M


object D extends B {
  override def foo(t: Int): Unit = {
    super.foo(t)
    println("M.foo")
  }
}


object Test {

  def main(args: Array[String]): Unit = {
    D.foo(42) // OK, prints B.foo M.foo
    C.foo(42) // was StackOverflowError
  }

}


