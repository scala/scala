class A {
  object b {
    object c
  }
  def m = b.c
}

object Test {
  var a: A = new A // mutable
  val c /*: object _1.b.c forSome { val _1: A } */ = a.m // widening using existential

  def mani[T: Manifest](x: T) = ()
  mani/*[object _1.b.c]*/(c) // kaboom in manifestOfType / TreeGen.mkAttributedQualifier
  // --> _1 is not in scope here
}