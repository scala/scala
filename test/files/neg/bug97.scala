object Main {
  def foo(x: Any) = x match {
    case List(y) => "zero"
    case y :: ys => "one"
  }
  def main(args: Array[String]) = System.out.println(List(1, 2));
}
