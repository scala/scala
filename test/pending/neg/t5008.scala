// These are members of class bar.C, completely unrelated to class foo.A.
// The types shown below include types defined within foo.A which are:
//
//   - qualified private
//   - qualified protected
//   - object protected
//
// val a : foo.A = { /* compiled code */ }
// val xprot1 : java.lang.Object with foo.A.FooProt1 = { /* compiled code */ }
// val xprot2 : java.lang.Object with foo.A.FooProt2 = { /* compiled code */ }
// val xprot3 : java.lang.Object with foo.A.FooProt3 = { /* compiled code */ }
// val xprot4 : java.lang.Object with foo.A.FooProt4 = { /* compiled code */ }
// val xpriv3 : java.lang.Object with foo.A.FooPriv3 = { /* compiled code */ }
// val xpriv4 : java.lang.Object with foo.A.FooPriv4 = { /* compiled code */ }
//
// Indeed it will tell me a type which I cannot access:
//
// scala> new bar.C
// res0: bar.C = bar.C@1339a0dc
//
// scala> res0.xpriv3
// res1: java.lang.Object with res0.a.FooPriv3 = bar.C$$anon$29@39556aec
//
// scala> new res0.a.FooPriv3
// <console>:9: error: trait FooPriv3 in class A cannot be accessed in foo.A
//               new res0.a.FooPriv3
//                          ^
// Looking at how the compiler prints the types of those vals, one
// develops a suspicion how some of it is being allowed:
//
// val xpriv4: C.this.a.FooPriv4
// val xpriv3: C.this.a.FooPriv3
// val xprot4: C.this.a.FooProt4
// val xprot3: C.this.a.FooProt3
// val xprot2: C.this.a.FooProt2
// val xprot1: C.this.a.FooProt1
//
// That is, "this" is in the prefix somewhere, it's just not a "this"
// which has any bearing.

package foo {
  class A {
    trait Foo

    protected trait FooProt1
    protected[this] trait FooProt2
    protected[foo] trait FooProt3
    protected[A] trait FooProt4

    private trait FooPriv1
    private[this] trait FooPriv2
    private[foo] trait FooPriv3
    private[A] trait FooPriv4

    type BarProt1 = FooProt1
    type BarProt2 = FooProt2
    type BarProt3 = FooProt3
    type BarProt4 = FooProt4

    // type BarPriv1 = FooPriv1
    // type BarPriv2 = FooPriv2
    type BarPriv3 = FooPriv3
    type BarPriv4 = FooPriv4

    def fprot1(x: FooProt1) = x
    def fprot2(x: FooProt2) = x
    def fprot3(x: FooProt3) = x
    def fprot4(x: FooProt4) = x

    // def fpriv1(x: FooPriv1) = x
    // def fpriv2(x: FooPriv2) = x
    def fpriv3(x: FooPriv3) = x
    def fpriv4(x: FooPriv4) = x

    val yprot1 = new FooProt1 { }
    val yprot2 = new FooProt2 { }
    val yprot3 = new FooProt3 { }
    val yprot4 = new FooProt4 { }

    // val ypriv1 = new FooPriv1 { }
    // val ypriv2 = new FooPriv2 { }
    val ypriv3 = new FooPriv3 { }
    val ypriv4 = new FooPriv4 { }

    def fpriv_alt1(x: FooPriv1) = 0 // !!! isn't the private type now in the signature of the (public) method?
    def fpriv_alt2(x: FooPriv2) = 0 // !!! isn't the private[this] type now in the signature of the (public) method?
  }
  // Same package, subclass
  class B extends A {
    val xprot1 = new BarProt1 { }
    val xprot2 = new BarProt2 { }
    val xprot3 = new BarProt3 { }
    val xprot4 = new BarProt4 { }

    // val xpriv1 = new BarPriv1 { }
    // val xpriv2 = new BarPriv2 { }
    val xpriv3 = new BarPriv3 { }
    val xpriv4 = new BarPriv4 { }

    override def fprot1(x: BarProt1) = x
    override def fprot2(x: BarProt2) = x
    override def fprot3(x: BarProt3) = x
    override def fprot4(x: BarProt4) = x

    // override def fpriv1(x: BarPriv1) = x
    // override def fpriv2(x: BarPriv2) = x
    override def fpriv3(x: BarPriv3) = x
    override def fpriv4(x: BarPriv4) = x
  }
  // Same package, unrelated class
  class C {
    val a = new A
    import a._

    val xprot1 = new BarProt1 { }
    val xprot2 = new BarProt2 { }
    val xprot3 = new BarProt3 { }
    val xprot4 = new BarProt4 { }

    // val xpriv1 = new BarPriv1 { }
    // val xpriv2 = new BarPriv2 { }
    val xpriv3 = new BarPriv3 { }
    val xpriv4 = new BarPriv4 { }
  }
}

package bar {
  // Different package, subclass
  class B extends foo.A {
    val xprot1 = new BarProt1 { }
    val xprot2 = new BarProt2 { }
    val xprot3 = new BarProt3 { }
    val xprot4 = new BarProt4 { }

    // val xpriv1 = new BarPriv1 { }
    // val xpriv2 = new BarPriv2 { }
    val xpriv3 = new BarPriv3 { }
    val xpriv4 = new BarPriv4 { }

    override def fprot1(x: BarProt1) = x
    override def fprot2(x: BarProt2) = x
    override def fprot3(x: BarProt3) = x
    override def fprot4(x: BarProt4) = x

    // override def fpriv1(x: BarPriv1) = x
    // override def fpriv2(x: BarPriv2) = x
    override def fpriv3(x: BarPriv3) = x
    override def fpriv4(x: BarPriv4) = x
  }
  // Different package, unrelated class
  class C {
    val a = new foo.A
    import a._

    val xprot1 = new BarProt1 { }
    val xprot2 = new BarProt2 { }
    val xprot3 = new BarProt3 { }
    val xprot4 = new BarProt4 { }

    // val xpriv1 = new BarPriv1 { }
    // val xpriv2 = new BarPriv2 { }
    val xpriv3 = new BarPriv3 { }
    val xpriv4 = new BarPriv4 { }
  }
}
