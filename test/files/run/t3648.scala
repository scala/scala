object Test {
  class C(val s: String = "")
  object C extends C() {
    override def toString() = "kult"
  }

  def main(args: Array[String]) {
    println(C)
  }
}
