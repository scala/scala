package tastytest

object TestDefAnnots extends Suite("TestDefAnnots") {

  test(assert(DefAnnots.withArgAnnot1("arg") === ("arg" : Any)))
  test(assert(DefAnnots.withParamInferAnnot("arg") === ("arg" : Any)))
  test(assert(DefAnnots.withAnnotatedAnnot("arg") === ("arg": Any)))

}
