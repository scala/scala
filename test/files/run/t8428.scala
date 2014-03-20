object Test extends App {
  val xs = List.tabulate(4)(List(_))
  val i = xs.map(_.iterator).reduce { (a,b) =>
    a.hasNext
    a ++ b
  }

  val r1 = i.toList
  val r2 = xs.flatten.toList

  assert(r1 == r2, r1)
}
