trait C {
  type t;
  def copy(x: t): t = x;
}

class D() extends C {
  type t = Int;
  System.out.println(copy(1));
}
