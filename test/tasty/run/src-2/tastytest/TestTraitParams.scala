package tastytest

object TestTraitParams extends Suite("TestTraitParams") {
  test(assert(TraitParams.foo.s === "I am Bar"))
  test(assert(TraitParams.foo.i === 23))
  test {
    val cls = new ClassExtendingTopLevelTraitWithParams()
    assert(cls.s == "I am ClassExtendingTopLevelTraitWithParams")
  }
  test {
    val cls = new ClassExtendingTopLevelTraitWithParams()
    assert(cls.i == 47)
  }
}
