package tastytest

object TestCaseClassDefault extends Suite("TestCaseClassDefault") {

  test(assert(CaseClassDefault.apply().value === 23))

  test {
    val i = new CaseClassDefault.Inner()
    assert(i.Local.apply().value === 47)
  }

  test(assert(CaseClassDefault.FakeCaseClass.apply().value === 97))

}
