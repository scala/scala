object A {
  case class B(c: C) {
    class C;
  }
  class C;
  val b: B = new B(new C());
  val c: C = b.c;
}
