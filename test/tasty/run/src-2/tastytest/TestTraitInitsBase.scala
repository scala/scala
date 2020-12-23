package tastytest

/** test calling $init$ on TraitInitsBase.SubTrait parent */
object TestTraitInitsBase extends Suite("TestTraitInitsBase") {

  object Static extends TraitInitsBase.SubTrait {}

  test("static SubTrait")(assert(Static.foo === 23))

  test("local SubTrait") {
    val Local = new TraitInitsBase.SubTrait {}
    assert(Local.foo === 23)
  }

}
