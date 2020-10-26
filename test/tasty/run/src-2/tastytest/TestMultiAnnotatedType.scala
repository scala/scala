package tastytest

object TestMultiAnnotatedType extends Suite("TestMultiAnnotatedType") {

  import MultiAnnotatedType.{annotA, annotB, annotC}

  test(assert(MultiAnnotatedType.listOfStrings.headOption === Option("foo")))
  test(assert(MultiAnnotatedType.id(23) === (23: @annotA @annotB @annotC)))

}
