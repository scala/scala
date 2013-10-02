/** Make sure that when a variable is captured its initialization expression is handled properly */
object Test {
  def lazyVal() = {
  	// internally lazy vals become vars which are initialized with "_", so they need to be tested just like vars do
  	lazy val x = "42"
    assert({ () => x }.apply == "42")
  }
  def ident() = {
    val y = "42"
    var x = y
    assert({ () => x }.apply == "42")
  }
  def apply() = {
    def y(x : Int) = x.toString
    var x = y(42)
    assert({ () => x }.apply == "42")
  }
  def literal() = {
    var x = "42"
    assert({ () => x }.apply == "42")
  }
  def `new`() = {
    var x = new String("42")
    assert({ () => x }.apply == "42")
  }
  def select() = {
    object Foo{val bar = "42"}
    var x = Foo.bar
    assert({ () => x }.apply == "42")
  }
  def `throw`() = {
    var x = if (true) "42" else throw new Exception("42")
    assert({ () => x }.apply == "42")
  }
  def assign() = {
    var y = 1
    var x = y = 42
    assert({ () => x}.apply == ())
  }
  def valDef() = {
    var x = {val y = 42}
    assert({ () => x}.apply == ())
  }
  def `return`(): String = {
    var x = if (true) return "42" else ()
    assert({ () => x}.apply == ())
    "42"
  }
  def tryFinally() = {
    var x = try { "42" } finally ()
    assert({ () => x }.apply == "42")
  }
  def tryCatch() = {
    var x = try { "42" } catch { case _: Throwable => "43" }
    assert({ () => x }.apply == "42")
  }
  def `if`() = {
  	var x = if (true) ()
    assert({ () => x }.apply == ())
  }
  def ifElse() = {
    var x = if(true) "42" else "43"
    assert({ () => x }.apply == "42")
  }
  def matchCase() = {
    var x = 100 match {
       case 100 => "42"
      case _ => "43"
    }
    assert({ () => x }.apply == "42")
  }
  def block() = {
    var x = {
      val y = 42
      "42"
    }
    assert({ () => x }.apply == "42")
  }
  def labelDef() = {
    var x = 100 match {
      case 100 => try "42" finally ()
    }
    assert({ () => x }.apply == "42")
  }
  def nested() = {
    var x = {
      val y = 42
        if(true) try "42" catch {case _: Throwable => "43"}
        else "44"
    }
    assert({ () => x }.apply == "42")
  }
  def main(args: Array[String]) {
  	lazyVal()
    ident()
    apply()
    literal()
    `new`()
    select()
    `throw`()
    assign()
    valDef()
    `return`()
    tryFinally()
    tryCatch()
    ifElse()
    `if`()
    matchCase()
    block()
    labelDef()
    nested()
  }
}

