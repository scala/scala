package tastytest

object TestSetters extends Suite("TestSetters") {

  test("mut.foo = 22") {
    val mut = (new Mutable).ensuring(_.foo === 33)
    mut.foo = 22
    assert(mut.foo === 22)
  }

  test("mut.foo_=(11)") {
    val mut = (new Mutable).ensuring(_.foo === 33)
    mut.foo_=(11)
    assert(mut.foo === 11)
  }

  test("mut.bar = 35") {
    val mut = (new Mutable).ensuring(_.bar === 45)
    mut.bar = 35
    assert(mut.bar === 35)
  }

  test("mut.bar_=(25)") {
    val mut = (new Mutable).ensuring(_.bar === 45)
    mut.bar_=(25)
    assert(mut.bar === 25)
  }

}
