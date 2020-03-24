package tastytest

object RecTypes {

  class C { type T1; type T2 }

  class CDep[C2 <: C { type T1; type T2 = T1 }] { // 1 level RECtype
    def test_==(c: C2)(c1: c.T1, c2: c.T2): Boolean = c1 == c2
  }

  class CDepNest[C2 <: C { type T1 <: C { type T1 = Int; type T2 = T1}; type T2 = T1 }] { // 2 level nested RECtype
    def test_==(c: C2)(cc1: c.T1, cc2: c.T2)(c1: cc1.T1, c2: cc1.T2, c3: cc2.T1, c4: cc2.T2): Boolean =
      c1 == c2 && c1 == c3 && c1 == c4 && c2 == c3 && c2 == c4 && c3 == c4
  }

}
