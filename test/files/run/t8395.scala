 object Test {
  def baz(x: Object) = {
    val s @ (_s: String) = x
    x
  }
  def main(args: Array[String]) {
    assert(baz("1") == "1")
  }
}
