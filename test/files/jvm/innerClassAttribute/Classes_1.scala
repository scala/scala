class A1 {
  class B
}

class A2 {
  object B
}

object A3 {
  class B1
  object B2
}

class A4 {
  def f(l: List[String]): List[String] = {
    l map (_ + "1")
  }
}

class A5 {
  def f(): Object = {
    object B
    B
  }
}

trait A6 {
  def hui = -6
  trait TT
}

class A7 extends A6

abstract class A8 extends A6 {
  def fish: TT
}

class A9 {
  class brick extends annotation.StaticAnnotation
}

class A10 {
  val a9 = new A9()
  // there's no reference to brick in the bytecode (only in the pickle), so there's no InnerClass attribute for it.
  @a9.brick def f = -7
}

class A11 {
  @JavaAnnot_1.Ann def f = -8
}

object A12 {
  object B {
    class C
  }
}

class A13 {
  def oak: A12.B.C = new A12.B.C
}

class A14 {
  def f = {
    val x: Object = {
      class K
      new K
    }
    x
  }
  def g = {
    val x: Object = new A6 { }
  }
}

object A15 {
  def f = {
    class B { // non-static, even though it doesn't have an outer pointer
      class C // non-static
    }
  }
}

class A16 {
  val x: A6 = {
    class U extends A6
    new A6 { }
  }

  {
    class V extends A6
    new A6 { }
  }

  new A6 { }
}

class A17 {
  object B {
    class C // not static, also has an outer pointer.
  }
}

class A18 {
  def f = {
    def g = {
      class A
      new A6 { }
      val y = {
        if ((new Object).hashCode() == 1) {class B {} ; new B} else 2
        if ((new Object).hashCode() == 1) new A6 { } else "haifish"
      }
    }
  }
}

class A19 {
  ((x: String) => x + "3")

  val x = {
    ((x: String) => x + "1")
  }

  {
    ((x: String) => x + "2")
  }
}

class A20 {
  (s: String) => {
    {(s: String) => ()}
    {(s: String) => (s: String) => 1}
  }
}

class A21 {
  class I1
  def f = { class J1 }
}
object A21 {
  class I2
  object I3 {
    class J2  // static
  }
  def g = { class J3 } // non-static
  val x = { class J4 } // non-static
  {
    class J5 // non-static (!)
    new J5
  }
}

class A22 {
  class C
  object C {
    class D // inner class of C$, not of C. Not added to the inner class table of C, only to C$
  }
}

class A23 {
  def f = {
    val a = new Java_A_1()
    val c = new Java_A_1.C()
    val d = new Java_A_1.C.D()
    val e = new c.E()
    val f = new a.F()
    val g = new f.G()
  }
}

trait A24Sym

trait A24Base {
  // trait with concrete members: interface plus (absract) impl class
  trait DefinitionsApi {
    def Abs: A24Sym
    def Conc: A24Sym = new A24Sym { }
  }
}

trait A24 extends A24Base {
  class DefinitionsClass extends DefinitionsApi {
    // bridge methods are generated for Abs and Conc. there used to be a bug: the bridge symbol was a ModuleSymbol,
    // calling companionClass would return NoSymbol. i changed erasure to make the bridge symbol is a MethodSymbol.
    object Abs extends A24Sym
    override object Conc extends A24Sym
  }
}

class SI_9105 {    
  // the EnclosingMethod attributes depend on the delambdafy strategy (inline vs method)

                                       //  outerClass-inline   enclMeth-inline   outerClass-method   enclMeth-method
  val fun = (s: String) => {
    class A                            //     closure             null (*)            SI_9105           null
    def m: Object = { class B; new B } //     closure              m$1                SI_9105            m$1
    val f: Object = { class C; new C } //     closure             null (*)            SI_9105           null
  }
  def met = (s: String) => {
    class D                            //     closure             null (*)            SI_9105            met
    def m: Object = { class E; new E } //     closure              m$1                SI_9105            m$1
    val f: Object = { class F; new F } //     closure             null (*)            SI_9105            met
  }

  // (*) the originalOwner chain of A (similar for D) is: SI_9105.fun.$anonfun-value.A
  //     we can get to the anonfun-class (created by uncurry), but not to the apply method.
  //
  //     for C and F, the originalOwner chain is fun.$anonfun-value.f.C. at later phases, the rawowner of f is
  //     an apply$sp method of the closure class. we could use that as enclosing method, but it would be unsystematic
  //     (A / D don't have an encl meth either), and also strange to use the $sp, which is a compilation artifact.
  //     So using `null` looks more like the situation in the source code: C / F are nested classes of the anon-fun, and
  //     there's no method in between.

  def byName(op: => Any) = 0

  val bnV = byName {
    class G                            //     closure             null (*)            SI_9105           null
    def m: Object = { class H; new H } //     closure              m$1                SI_9105            m$1
    val f: Object = { class I; new I } //     closure             null (*)            SI_9105           null
    ""
  }
  def bnM = byName {
    class J                            //     closure             null (*)            SI_9105            bnM
    def m: Object = { class K; new K } //     closure              m$1                SI_9105            m$1
    val f: Object = { class L; new L } //     closure             null (*)            SI_9105            bnM
    ""
  }
}

trait SI_9124 {
  trait A // member class, no enclosing method attribute

  new A { def f1 = 0 } // nested class, enclosing class SI_9124, no encl meth

  def f = new A { def f2 = 0 } // enclosing method is f in the interface SI_9124

  private def g = new A { def f3 = 0 } // only encl class (SI_9124), encl meth is null because the interface SI_9124 doesn't have a method g

  object O { // member, no encl meth attribute
    new A { def f4 = 0 } // enclosing class is O$, no enclosing method
  }

          val f1 = { new A { def f5 = 0 }; 1 } // encl class SI_9124, no encl meth
  private val f2 = { new A { def f6 = 0 }; 1 } // like above
}

trait ImplClassesAreTopLevel {
  // all impl classes are top-level, so they don't appear in any InnerClass entry, and none of them have an EnclosingMethod attr
  trait B1 { def f = 1 }
  { trait B2 { def f = 1 }; new B2 {} }
  val m = {
    trait B3 { def f = 1 }
    new B3 {}
  }
  def n = {
    trait B4 { def f = 1 }
    new B4 {}
  }
}

class SpecializedClassesAreTopLevel {
  // all specialized classes are top-level
  class A[@specialized(Int) T]; new A[Int]

  object T {
    class B[@specialized(Int) T]; new B[Int]
  }

  // these crash the compiler, SI-7625

  // { class B[@specialized(Int) T]; new B[Int] }

  // val m: Object = {
  //   class C[@specialized(Int) T]
  //   new C[Int]
  // }

  // def n: Object = {
  //   class D[@specialized(Int) T]
  //   new D[Int]
  // }
}

object NestedInValueClass {
  // note that we can only test anonymous functions, nested classes are not allowed inside value classes
  class A(val arg: String) extends AnyVal {
    // A has InnerClass entries for the two closures (and for A and A$). not for B / C
    def f = {
      def g = List().map(x => ((s: String) => x)) // outer class A, no outer method (g is moved to the companion, doesn't exist in A)
      g.map(x => ((s: String) => x))              // outer class A, outer method f
    }
    // statements and field declarations are not allowed in value classes
  }

  object A {
    // A$ has InnerClass entries for B, C, A, A$. Also for the closures above, because they are referenced in A$'s bytecode.
    class B // member class of A$
    def f = { class C; new C } // outer class A$, outer method f
  }
}
