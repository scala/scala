object Test {
  def main(args: Array[String]): Unit = {
    val traversable = 1 to 20 map (_.toString)
    def normalize(m: Map[Char, Traversable[String]]) = m.map { case (k,v) => (k, v.toList) }

    val groupedFromView   = (traversable view).groupBy(_(0))
    val groupedFromStrict = traversable.groupBy(_(0))

    assert(normalize(groupedFromView) == normalize(groupedFromStrict))
  }
}
