// scalac: -deprecation -Xfatal-warnings -Xsource:2.14

object Test1 {
  val im1 = scala.collection.immutable.Map("foo" -> 1).updated("bar", 2) // ok
  val im2 = scala.collection.immutable.Map("foo" -> 1) updated ("baz", 3) // not ok
}
