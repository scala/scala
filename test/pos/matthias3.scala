
abstract class A() {
    val y: A;
}
class B() extends A() {
    val x = this;
    val y: x.type = x;
}
abstract class C() {
    val b: B = new B();
    val a: A { val y: b.type };
}

