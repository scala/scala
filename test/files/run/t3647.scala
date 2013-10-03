


import collection.immutable._


object Test {
  def main(args: Array[String]) {
    val ps = PagedSeq.fromLines(List(
      "line1",
      "line2",
      "line3",
      "line4"
    ).iterator)
    assert(ps.filter(_ == '\n').size == 3)

    val ps1 = PagedSeq.fromLines(List("Ok").iterator)
    assert(ps1.filter(_ == '\n').size == 0)

    val eps = PagedSeq.fromLines(List().iterator)
    assert(eps.filter(_ == '\n').size == 0)
  }
}
