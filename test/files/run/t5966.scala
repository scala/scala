object o { def apply(i: AnyRef*)(j: String) = i }

object Test {
  def main(args: Array[String]) {
    println("(o()_)(\"\") = " + (o()_)(""))
    println("(o(\"a1\")_)(\"\") = " + (o("a1")_)(""))
    println("(o(\"a1\", \"a2\")_)(\"\") = " + (o("a1", "a2")_)(""))
  }
}
