object Bug {
  class A;
  case class A1 extends A;
  case class A2 extends A;
  def f: A =
    if (true)
      A1()
    else {
      val a = if (true) A1() else A2();
      a
    };
}
