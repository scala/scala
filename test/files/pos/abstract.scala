abstract class C() {
  type t;
  def copy(x: t): t = x;
}

class D() extends C() {
  type t = Int;
  Console.println(copy(1));
}
