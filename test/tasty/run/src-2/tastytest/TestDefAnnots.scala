package tastytest

object TestDefAnnots extends Suite("TestDefAnnots") {

  test(assert(DefAnnots.withArgAnnot1("arg") === ("arg" : Any)))

}
