package tastytest

object TestAnnotated {
  def test2 = forceAnnots[RootAnnotated, rootAnnot, "new tastytest.rootAnnot(1)"]
  def test3 = {
    forceAnnots[
      OuterClassAnnotated,
      basicAnnot[String],
      "new  <: tastytest.basicAnnot[String](OuterClassAnnotated.this.xyz)"
    ]
  }
  def test4 = {
    forceAnnots[
      ParameterizedAnnotated,
      basicAnnot[Int],
      "new  <: tastytest.basicAnnot[Int](tastytest#ParameterizedAnnotated.type.value)"
    ]
  }
  def test5 = {
    val o = new OuterAnnotated {}
    forceAnnots[OuterAnnotated, o.innerAnnot, "new OuterAnnotated.this.innerAnnot(new Inner())"]
  }
  def test6 = {
    forceAnnots[
      SelectInAnnotated.AmbiguousAnnotated,
      SelectInAnnotated.ambig.annot,
      "new tastytest.SelectInAnnotated.ambig.annot(tastytest.SelectInAnnotated.e.type)"
    ]
  }
  def test7 = {
    forceAnnots[
      SelectInAnnotatedinParent.AmbiguousAnnotated,
      SelectInAnnotatedinParent.ambig.annotBox,
      "new tastytest.SelectInAnnotatedinParent.ambig.annotBox(0.0)"
    ]
  }
}
