object Test {
  def test1: Unit = {
    C
    object C
  }

  def test2: Unit = {
    println(s.length)
    lazy val s = "abc"
  }

  def test3: Unit = {
    lazy val lazyBar = bar
    object bar {
      val foo = 12
    }
    lazy val lazyBar2 = bar
  }

  def test4: Unit = {
    lazy val x = {
      x
      lazy val x = 12
      0
    }
  }

  def test5: Unit = {
    lazy val f: Int = g
    Console.println("foo")
    lazy val g: Int = f
  }
}
