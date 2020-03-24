package tastytest

object RecTypes {
  class C { type T1; type T2 }
  class CDep[C2 <: C { type T1; type T2 = T1 }] {
    def test_==(c: C2)(c1: c.T1, c2: c.T2): Boolean = c1 == c2
  }
}
