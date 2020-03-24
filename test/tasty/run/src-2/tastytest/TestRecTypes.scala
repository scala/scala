package tastytest

object TestRecTypes extends Suite("TestRecTypes") {

  class CInt extends RecTypes.C {
    type T1 = Int
    type T2 = Int
  }

  class CCInt extends RecTypes.C {
    type T1 = CInt
    type T2 = CInt
  }

  test("1 level RECtype") {
    val cdepTest = new RecTypes.CDep[CInt].test_==(new CInt)
    assert(cdepTest(1,1) === true)
    assert(cdepTest(1,2) === false)
  }

  test("2 level nested RECtype") {
    val cint = new CInt
    val cdepNestTest = new RecTypes.CDepNest[CCInt].test_==(new CCInt)(cint, cint)
    assert(cdepNestTest(1,1,1,1) === true)
    assert(cdepNestTest(2,1,1,1) === false)
    assert(cdepNestTest(1,2,1,1) === false)
    assert(cdepNestTest(1,1,2,1) === false)
    assert(cdepNestTest(1,1,1,2) === false)
  }

}
