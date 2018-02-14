object Test {

  def main(args: Array[String]): Unit = {
    println(foo(true))
    println(foo(false))
  }

  def foo(b: Boolean): String = {
    try {
      if(b)
        return "Hello"
      else
        "abc"
    } finally {
      10 match {case x => ()}
    }
  }

}

