package tastytest

object TestFromJavaObjectConsume {

  def test1 = new FromJavaObjectConsume.Foo {}

  def test2 = {
    forceAnnots[
      FromJavaObjectConsume.Foo,
      basicAnnot[Any],
      "new  <: tastytest.basicAnnot[Int](tastytest#FromJavaObjectBox.type.id[Int](23))"
    ]
  }

}
