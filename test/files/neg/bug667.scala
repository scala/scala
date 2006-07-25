package test;

object test {
  trait A {
    trait Ni;
  }
  class B extends A {
    class Ni extends super.Ni with Ni;
  }
}
