// scalac: -Xmigration -Werror
object Test {
  val f = Equiv[Float]
  val d = Equiv[Double]

  // In our similar test for `Ordering`, we check that the migration warning is produced
  // when e.g. calling `sorted` on a list.  I wanted to add something like that here,
  // but I couldn't find anywhere where `Equiv` is actually used, except by
  // `scala.collection.concurrent.TrieMap`, but that class defaults to using
  // the (deprecated) `Equiv.universal`, so you don't run into the migration warning
  // normally.
}
