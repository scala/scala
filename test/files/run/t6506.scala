object Test {
  def main(args: Array[String]) {
    new WL(new {} #:: S) with T
  }
  object S { def #::(a: Any): Any = () }
  trait T
  class WL(a: Any)
}
