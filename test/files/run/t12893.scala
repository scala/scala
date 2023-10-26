
object Test extends App {
  val s"Hello,\t$name" = s"Hello,\tJames"
  assert(name == "James")
  assert(StringContext.glob(Seq("Hello,\t", ""), "Hello,\tJames") == Some(Seq("James")))
}
