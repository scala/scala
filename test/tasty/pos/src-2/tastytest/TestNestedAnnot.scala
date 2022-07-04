package tastytest

object TestNestedAnnot {
  def test = {
    val _ = new TaggedMega.Tagged()
    compiletimeHasChild[TaggedMega.ForceChildren]("tastytest.TaggedMega.Nested.Nested2.Tags")
    forceAnnots[TaggedMega.Tagged, TaggedMega.Nested.Nested2.Tags, "new tastytest#TaggedMega.Nested.Nested2.Tags(\"xyz,foo\")"]
  }
}
