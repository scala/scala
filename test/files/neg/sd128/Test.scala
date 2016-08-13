trait T {
  def f = 99
}
class C1 extends A with T // error
class C2 extends T with A // error

trait U extends A {
  override def f = 999
}
class D1 extends A with U // OK
class D2 extends U with A // OK

class E1 extends A {
  def f() = 9999 // need override modifier
}

class E2 extends A {
  override def f() = 9999 // OK
}
