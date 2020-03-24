package tastytest

object TestRecTypes extends Suite("TestRecTypes") {
  class CInt extends RecTypes.C {
    type T1 = Int
    type T2 = Int
  }
  test {
    val cdepTest = new RecTypes.CDep[CInt].test_==(new CInt)
    assert(cdepTest(1,1) === true)
    assert(cdepTest(1,2) === false)
  }
}
