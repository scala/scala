//> using options -Werror -Xlint:recurse-with-default
object Test {
  def rec1(a: Any, b: Any = "".reverse): Any = {
    rec1(0, 0)  // okay
    rec1(0)     // warn
  }

  def rec2(a: Any, b: Any = "".reverse): Any = {
    def nested = {
      rec2(0)   // warn
    }
    object X {
      rec2(0)   // warn
    }
    class X {
      rec2(0)   // warn
    }
  }


  def rec3(a: Any) = ()
  def rec3(a: Any, b: Any = "".reverse): Any = {
    rec3(0)     // okay
  }

  def rec4(a: Any)(b: Any = "".reverse): Any = {
    rec4(0)()   // warn
  }

  def rec5(a: Any)(b: Any = 0): Any = {
    rec5(0)()   // warn
  }

  def rec6(a: Any = 0)(b: Any = 0): Any = {
    rec6()(0)   // warn
  }

  def rec7(a: Any = 0)(b: Any = 0): Any = {
    rec7()(0)   // one warning only
  }
}
