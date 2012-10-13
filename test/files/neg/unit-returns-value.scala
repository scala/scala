object Test {
  def f {
    var b = false
    if (b) return 5
  }

  // no warning
  def g {
    return println("hello")
  }
}

class UnusedValues {
  var i1 = 2
  val i2 = 2
  lazy val i3 = 2
  object i4 { }
  def i5 = 2
  final def i6 = 2

  def x = {
    i1 // warn
    i2 // warn
    i3 // no warn
    i4 // no warn
    i5 // no warn
    i6 // could warn someday, if i6 returned 2.type instead of Int

    5
  }
}

