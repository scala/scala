package tastytest

object TestDefAnnots extends Suite("TestDefAnnots") {

  test(assert(DefAnnots.withArgAnnot1("arg") === ("arg" : Any)))
  test(assert(DefAnnots.withParamInferAnnot("arg") === ("arg" : Any)))
  test(assert(DefAnnots.withAnnotatedAnnot("arg") === ("arg": Any)))

  def compiletimeAsserts = {
    def test1 = {
      forceAnnots[
        DefAnnots.type,
        DefAnnots.inferAnnot[Any],
        "new  <: tastytest.DefAnnots.inferAnnot[tastytest.DefAnnots.Inner.Foo.type](tastytest.DefAnnots.Inner.type.Foo)"
      ]
    }
    def test2 = {
      forceAnnots[
        DefAnnots.type,
        DefAnnots.Wrapper.annotatedAnnot,
        "new tastytest.DefAnnots.Wrapper.annotatedAnnot()"
      ]
    }
    def test3 = {
      forceAnnots[
        DefAnnots.Wrapper.annotatedAnnot,
        DefAnnots.Wrapper.annot,
        "new tastytest.DefAnnots.Wrapper.annot()"
      ]
    }
  }

}
