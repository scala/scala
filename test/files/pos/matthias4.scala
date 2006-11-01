/*
object A requires B {
    B.X getX() {
        return B.getX();
    }
    void setX(B.X x) {}
}
object B {
    class X {}
    X getX() {
        return new X();
    }
    void setX(X x) {}
}
object C requires B {
    object A;
    void test() {
        A.setX(B.getX());
    }
}
*/

trait _a extends AnyRef with _b {
    val a: _a;
    val A: A;
    type A <: a.AObject;
    trait AObject {
        def getX(): B.X;
        def setX(x: B.X): Unit;
    }
}
trait a123 extends AnyRef with _a with _b {
    val a: this.type = this;
    val A: A = new A();
    class A() extends AObject {
        def getX(): B.X = B.getX();
        def setX(x: B.X) = B.setX(x);
    }
}

trait _b {
    val b: _b;
    val B: B;
    type B <: b.BObject;
    trait BObject {
        type X;
        def getX(): X;
        def setX(x: X): Unit;
    }
}
abstract class b() extends AnyRef with _b {
    val b: this.type = this;
    val B: B = new B();
    class B() extends BObject {
        class X() {}
        def getX(): X = new X();
        def setX(x: X) = ();
    }
}

trait _m {
    val m: _m;
    val M: M;
    type M <: m.MObject;
    trait MObject {}
}
abstract class m() extends AnyRef with _m with _b {
    val m: this.type = this;
    val M: M = new M();
    class M() extends MObject with a123 with Linker {
        def test() = {
            val x: B.X = B.getX();
            A.setX(x);
        }
    }
    trait Linker {
        val b: m.this.b.type = m.this.b;
        val B: m.this.B.type = m.this.B;
        type B = m.this.B;
        val m: m.this.m.type = m.this.m;
        val M: m.this.M.type = m.this.M;
        type M = m.this.M;
    }
}
