class A(val f : String)

class B(f: String) extends A(f) {
  def foo(x: String) = x match { 
    case `f`  => true
    case _    => false
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val b = new B("abc")
    println(b foo "bippy")
    println(b foo "abc")
  }
}
