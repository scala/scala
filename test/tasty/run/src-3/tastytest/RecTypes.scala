package tastytest

object RecTypes {

  class C { type T1; type T2 }

  class CDep[C2 <: C { type T1; type T2 = T1 }] { // 1 level RECtype using a TYPEREF
    def test_==(c: C2)(c1: c.T1, c2: c.T2): Boolean = c1 == c2
  }

  abstract class D { val d: Any; type T }

  class DDep[D2 <: D { val d: Int; type T = d.type }] { // 1 level RECtype using a TERMREF
    def test_==(d: D2)(t: d.d.type): Boolean = d.d == t
  }

  // abstract class E { type T; def poly[C2 <: C { type T1 = T; type T2 = T1 }](c: C2): T }

  // class EDep[E2 <: E { type T; def poly[C2 <: C { type T1 = T; type T2 = T1 }](c: C2): T }]

  class CDepNest[C2 <: C { type T1 <: C { type T1 = Int; type T2 = T1}; type T2 = T1 }] { // other example, not nested on the `spine`
    def test_==(c: C2)(cc1: c.T1, cc2: c.T2)(c1: cc1.T1, c2: cc1.T2, c3: cc2.T1, c4: cc2.T2): Boolean =
      c1 == c2 && c1 == c3 && c1 == c4 && c2 == c3 && c2 == c4 && c3 == c4
  }

}
