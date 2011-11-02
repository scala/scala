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
