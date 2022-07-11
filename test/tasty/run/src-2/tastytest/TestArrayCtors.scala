package tastytest

import ArrayCtors._

object TestArrayCtors extends Suite("TestArrayCtors") {

  test(assert(EmptyArrayCtor != null))
  test(assert(EmptyArrayCtor2 != null))

  def compiletimeAsserts = {
    def test1 = forceAnnots[
      ArrayCtors.EmptyArrayCtor.type,
      ArrayCtors.arrayAnnot,
      "new tastytest.ArrayCtors.arrayAnnot(Array.type.apply[tastytest.ArrayCtors.Module.type.type]((Array[tastytest.ArrayCtors.Module.type]{}: tastytest.ArrayCtors.Module.type*))(reflect#ClassTag.type.apply[tastytest.ArrayCtors.Module.type](classOf[tastytest.ArrayCtors$$Module])))"
    ]
    def test2 = forceAnnots[
      ArrayCtors.EmptyArrayCtor2.type,
      ArrayCtors.arrayAnnot2,
      "new tastytest.ArrayCtors.arrayAnnot2(Array.type.apply[Array[tastytest.ArrayCtors.Module.type.type]]((Array[Array[tastytest.ArrayCtors.Module.type]]{}: Array[tastytest.ArrayCtors.Module.type]*))(reflect#ClassTag.type.apply[tastytest.ArrayCtors.Module.type](classOf[tastytest.ArrayCtors$$Module]).wrap))"
    ]
  }

}
