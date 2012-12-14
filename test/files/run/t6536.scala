
// this was what was broken
class AClass {
  def a = "super class"
}
class BClass extends AClass {
  class M {
    def z = "child " + BClass.super[AClass].a 
 }
  override def a = ""
}

// this variant would crash the compiler
class O3 { def f = "" } // O3 must be a class
class O4 extends O3 {
  class T1 { def g = O4.super[O3].f } // ok
  trait T2 { def g = O4.super[O3].f } // crash
}

// make sure the fix didn't break this case, which wasn't broken
trait ATrait {
  def a = "super trait"
}
class BTrait extends ATrait {
  class M {
    def z = "child " + BTrait.super[ATrait].a
 }
  override def a = ""
}

// make sure the fix didn't break the simplest case
class AClass2 {
	def a = "super class 2"
}
class BClass2 extends AClass2 {
	override def a = ""
	def z = "child " + super.a
}

// make sure the fix didn't break this simplest case
class ATrait2 {
	def a = "super trait 2"
}
class BTrait2 extends ATrait2 {
	override def a = ""
	def z = "child " + super.a
}

// a more interesting example of the all that
// this was what was broken
class AClass3 {
  def a = "super class 3"
}
trait ATrait3a {
	def a = "super trait 3a"
}
trait ATrait3b {
	def a = "super trait 3b"
}
class BClass3 extends AClass3 with ATrait3a with ATrait3b {
  class M {
    def zclass = "child " + BClass3.super[AClass3].a
    def ztraita = "child " + BClass3.super[ATrait3a].a
    def ztraitb = "child " + BClass3.super[ATrait3a].a
 }
  override def a = ""
}

// here's a case where we call super from a trait
trait Root {
	def a = "root"
}

trait Mid extends Root {
	override def a = "mid"
	def b = super.a
}

class Bottom extends Mid

// and this is a bunch of other stuff we want to make sure doesn't explode
trait A1 { def m1 = "a1" }
trait A2 { def m1 = "a2" }

trait O1 { def m2 = "o1" ; def o1 = "o1" }
trait O2 { def m2 = "o2" }

class C1 { def m3 = "c1" }
trait C2 { def m3 = "c2" }

package outertrait {
  trait Outer extends O1 with O2 {
    override def m2 = "outertrait.Outer"

    trait A3_T extends A1 with A2 {
      override def m1 = "a3_t"

      def f1 = super[A1].m1
      def f2 = super[A2].m1
      def f3 = m1
      def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
      def f5 = Outer.super[O2].m2
      def f6 = Outer.this.m2 + "/" + Outer.this.o1
      def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
    }

    class A3_C extends A1 with A2 {
      override def m1 = "a3_c"

      def f1 = super[A1].m1
      def f2 = super[A2].m1
      def f3 = m1
      def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
      def f5 = Outer.super[O2].m2
      def f6 = Outer.this.m2 + "/" + Outer.this.o1
      def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
    }

    object A3_O extends A1 with A2 {
      override def m1 = "a3_o"

      def f1 = super[A1].m1
      def f2 = super[A2].m1
      def f3 = m1
      def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
      def f5 = Outer.super[O2].m2
      def f6 = Outer.this.m2 + "/" + Outer.this.o1
      def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
    }

    class A3_TC extends A3_T { override def m1 = "a3_tc" }
    object A3_TO extends A3_T { override def m1 = "a3_to" }
    object A3_TCO extends A3_TC { override def m1 = "a3_tco" }
  }
}

package outerclass {
  class Outer extends O1 with O2 {
    override def m2 = "outerclass.Outer"

    trait A3_T extends A1 with A2 {
      override def m1 = "a3_t"

      def f1 = super[A1].m1
      def f2 = super[A2].m1
      def f3 = m1
      def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
      def f5 = Outer.super[O2].m2
      def f6 = Outer.this.m2 + "/" + Outer.this.o1
      def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
    }

    class A3_C extends A1 with A2 {
      override def m1 = "a3_c"

      def f1 = super[A1].m1
      def f2 = super[A2].m1
      def f3 = m1
      def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
      def f5 = Outer.super[O2].m2
      def f6 = Outer.this.m2 + "/" + Outer.this.o1
      def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
    }

    object A3_O extends A1 with A2 {
      override def m1 = "a3_o"

      def f1 = super[A1].m1
      def f2 = super[A2].m1
      def f3 = m1
      def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
      def f5 = Outer.super[O2].m2
      def f6 = Outer.this.m2 + "/" + Outer.this.o1
      def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
    }

    class A3_TC extends A3_T { override def m1 = "a3_tc" }
    object A3_TO extends A3_T { override def m1 = "a3_to" }
    object A3_TCO extends A3_TC { override def m1 = "a3_tco" }
  }
}

package withinmethod {
  trait Outer extends O1 with O2 {
    override def m2 = "withinmethod.Outer"

    def method1 = {
      trait A3_T extends A1 with A2 {
        override def m1 = "a3_t"

        def f1 = super[A1].m1
        def f2 = super[A2].m1
        def f3 = m1
        def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
        def f5 = Outer.super[O2].m2
        def f6 = Outer.this.m2 + "/" + Outer.this.o1
        def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
      }
      class A3_C extends A1 with A2 {
        override def m1 = "a3_c"

        def f1 = super[A1].m1
        def f2 = super[A2].m1
        def f3 = m1
        def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
        def f5 = Outer.super[O2].m2
        def f6 = Outer.this.m2 + "/" + Outer.this.o1
        def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
      }
      object A3_O extends A1 with A2 {
        override def m1 = "a3_o"

        def f1 = super[A1].m1
        def f2 = super[A2].m1
        def f3 = m1
        def f4 = Outer.super[O1].m2 + "/" + Outer.super[O1].o1
        def f5 = Outer.super[O2].m2
        def f6 = Outer.this.m2 + "/" + Outer.this.o1
        def f7 = () => List(super[A1].m1, Outer.super[O1].m2)
      }
      class A3_TC extends A3_T { override def m1 = "a3_tc" }
      object A3_TO extends A3_T { override def m1 = "a3_to" }
      object A3_TCO extends A3_TC { override def m1 = "a3_tco" }

      List[Test.Anything](new A3_T { }, new A3_C, A3_O, new A3_TC, A3_TO, A3_TCO)
    }
  }
}

object Test {
  type Anything = {
    def f1: Any
    def f2: Any
    def f3: Any
    def f4: Any
    def f5: Any
    def f6: Any
    def f7: () => Any
  }

  def show(x: Anything) {
    import x._
    println(List(f1, f2, f3, f4, f5, f6, f7()) mkString " ")
  }
  def main(args: Array[String]): Unit = {
    {
      val o1 = new outertrait.Outer { }
      show(new o1.A3_T { })
      show(new o1.A3_C)
      show(o1.A3_O)
      show(new o1.A3_TC)
      show(o1.A3_TO)
      show(o1.A3_TCO)
      println("")
    }

    {
      val o1 = new outerclass.Outer { }
      show(new o1.A3_T { })
      show(new o1.A3_C)
      show(o1.A3_O)
      show(new o1.A3_TC)
      show(o1.A3_TO)
      show(o1.A3_TCO)
      println("")
    }

    {
      val o1 = new withinmethod.Outer { }
      o1.method1 foreach show
    }

  	val bclass = new BClass
  	val bclassm = new bclass.M
  	println(bclassm.z)

  	val btrait = new BTrait
  	val btraitm = new btrait.M
  	println(btraitm.z)

  	val bclass2 = new BClass2
  	println(bclass2.z)

  	val btrait2 = new BTrait2
  	println(btrait2.z)

  	val bclass3 = new BClass3
  	val bclass3m = new bclass3.M
  	println(bclass3m.zclass)
  	println(bclass3m.ztraita)
  	println(bclass3m.ztraitb)

  	val bottom = new Bottom
  	bottom.a
  	bottom.b
  }
}