class C {
  def id[A](r: A): A = r
  def bug(x: Int, e: Boolean): Unit = {
    x match {
      case 1 => id(())
      case 2 if e =>
    }
    println()
  }
}

class D {
  def foo(): Unit = ()
  def b: Boolean = false

  def bug(x: Int): Unit = {
    (x: @annotation.switch) match {
      case 2 if b =>
        foo()
      case _ if b =>
        foo()
      case _ =>
        foo()
    }
  }
}
