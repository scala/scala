object Bug359 {
  class C;
  def f1(xs: List[C]): C = {
    g {
      xs =>
        if (false) {
          f1(xs)
        } else {
          val a: C = null;
          val b: C = null;
          if (xs.isEmpty) a else b
        }
    }
  }
  def f2(xs: List[C]): C = {
    g {
      xs =>
        if (false) {
          val a: C = null;
          val b: C = null;
          if (xs.isEmpty) a else b
        } else {
          f2(xs);
        }
    }
  }
  private def g(op: List[C] => C): C = null;
}
