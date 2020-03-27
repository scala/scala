package tastytest

object TestRecTypes extends Suite("TestRecTypes") {

  class CInt extends RecTypes.C {
    type T = Int
    type U = Int
    val t: T = 23
    val u: U = 23
  }

  class DInt extends RecTypes.D {
    val d: Int = 2
    type T = d.type
    val t: T = d
  }

  class CCInt extends RecTypes.C {
    type T = CInt
    type U = CInt
    val t: T = new CInt
    val u: U = t
  }

  test("1 level RECthis, using a TYPEREF") {
    assert(new RecTypes.CDep[CInt].test_==(new CInt) == true)
  }

  test("2 level nested RECthis, using a TYPEREF") {
    assert(new RecTypes.CDepNest[CInt].test_==(new CInt) == true)
  }

  test("2 level nested RECthis, using a TYPEREF and projection") {
    assert(new RecTypes.CDepNestProject[Int].widenU(23) == 23)
  }

  test("RECtype within a refinement within a RECtype") {
    assert(new RecTypes.CDepDeep[CCInt].test_==(new CCInt) == true)
  }

  test("1 level RECthis, using a TERMREF") {
    assert(new RecTypes.DDep[DInt].test_==(new DInt) == true)
  }

  test("2 level nested RECthis, using a TERMREF") {
    assert(new RecTypes.DDepNest[DInt].test_==(new DInt) == true)
  }

  test("2 level nested RECthis, using a TERMREF and projection") {
    val dInt = new DInt
    assert(new RecTypes.DDepNestProject[dInt.T].widenT(dInt.d) == 2)
    assert(new RecTypes.DDepNestProject[dInt.t.type].widenT(dInt.t) == 2)
    assert(new RecTypes.DDepNestProject[dInt.d.type].widenT(dInt.t) == 2)
  }

  class EInt extends RecTypes.E {
    type T = d.type
    type U = e.type
    val d: Int = 23
    val e: Int = 23
  }

  test("1 level RECthis, twice, using a TYPEREF") {
    assert(new RecTypes.EDep[EInt].test_==(new EInt) == true)
  }

  class TInt {
    type T = Int
    val t: T = 23
  }

  test(assert(new RecTypes.StructuralTypeAlias[TInt].get(new TInt) === 23))
  test(assert(new RecTypes.StructuralTypeBounds[TInt].get(new TInt) === 23))

}
