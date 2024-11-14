class Bug1955 {
  var result: Int = 0

  def doSomething[A](a: Int, b: Int, r: A): A = {
    result = a + b
    r
  }

  def bug(x: Int, e: Boolean): Unit = {
    x match {
      case 1 => doSomething(123, 456, ())
      case 2 if e =>
    }

    if (false) ()
  }
}
