//############################################################################
// Test nested classes
//############################################################################

// The following set of classes tests nasty references to "outer"
// values.

class A(pa : Int) {
  def a1 = pa;
  class B(pb : Int) {
    def b1 = pa+pb+a1;
    class C(pc : Int) extends A(b1) {
      def c1 = pc+pb+pa
    }
    val c1 = new C(13)
  }
}

trait M {
  def m1 = 1
}

class A1(x : Int) extends A(x) with M {
  class D extends B(14) {
    val c2 = new C(15);
    class E extends C(16) {
      def e1 = c1+b1+a1+m1;
      def e2 = new D();
    }
  }
}

// The following set of classes test qualified "this" and "super"
// references.

class AA {
  def m = 1;
  class BB {
    def m = 2;
    class CC {
      def m = 3;
      def am = AA.this.m;
      def bm = BB.this.m;
    }
  }
}

class AAA {
  def f = 42;
}

class BBB extends AAA {
  override def f = 24;
}

class AAA1 extends AAA {
  override def f = 111;
  class BBB1 extends BBB {
    override def f = AAA1.super.f;
  }
  class BBB2 extends BBB {
    override def f = BBB2.super.f;
  }
  class BBB3 extends BBB {
    override def f = super.f;
  }
  class BBB4 extends BBB { }
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new A1(12);
    val d = new a.D;
    val e = new d.E;
    Console.println("e.e1 = " + e.e1);

    val aa = new AA;
    val bb = new aa.BB;
    val cc = new bb.CC;
    Console.println("cc.m = " + cc.m);
    Console.println("cc.am = " + cc.am);
    Console.println("cc.bm = " + cc.bm);

    val aaa = new AAA1;
    val bbb1 = new aaa.BBB1;
    val bbb2 = new aaa.BBB2;
    val bbb3 = new aaa.BBB3;
    val bbb4 = new aaa.BBB4;
    Console.println("aaa.f = " + aaa.f);
    Console.println("bbb1.f = " + bbb1.f);
    Console.println("bbb2.f = " + bbb2.f);
    Console.println("bbb3.f = " + bbb3.f);
    Console.println("bbb4.f = " + bbb4.f);
  }
}

//############################################################################
