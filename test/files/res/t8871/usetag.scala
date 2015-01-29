trait Foo

object Test {
  val y = new Tag().apply[Double, Foo](3.3)
  // under FSC, this gave t8871/usetag.scala:4: error: wrong number of type parameters for method apply$mDc$sp: [T](a: Double)Double
}
