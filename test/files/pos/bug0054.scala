class A {
  case class B(x: C) extends A { val z: A.this.C = x }
  class C {}
}
