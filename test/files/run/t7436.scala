class A(val p: Int*)

class B(val p1: Int) extends A(p1)

object Test {
  def main(args: Array[String]) {
    new B(1).p1 // threw java.lang.ClassCastException: scala.collection.mutable.WrappedArray$ofInt cannot be cast to java.lang.Integer
  }
}
