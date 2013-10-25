trait OuterTrait {
  trait InnerTrait {
    type Element
    type Collection <: Iterable[Inner.Element]
  }

  val Inner: InnerTrait

}

object OuterObject extends OuterTrait {
  object Inner extends InnerTrait {
    type Element = String
    override type Collection = Seq[Inner.Element]
  }
}
