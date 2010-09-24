object Test {
  def meh: Unit = {
    trait TC[I]
    implicit val tci = new TC[Int]{}

    def baz[J : TC] : String = "meh"

    baz
    // () // commenting or uncommenting this line should not affect compilation (visibly)
  }
}