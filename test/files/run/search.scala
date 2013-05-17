object Test extends App {
  import scala.collection.{LinearSeq, IndexedSeq}
  import scala.collection.Searching.search

  val ls = LinearSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13)
  println(ls.search(3))
  println(ls.search(5, 3, 8))
  println(ls.search(12))

  val is = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13)
  println(is.search(3))
  println(is.search(5, 3, 8))
  println(is.search(12))
}
