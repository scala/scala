class A() {
     class B() {
         def foo(x: B) = 0
     }
}
object test {
  def main = {
    val a = new A();
    val b = new a.B();
    val c = new a.B();
    val d = b.foo(c);
    ()
  }
}

