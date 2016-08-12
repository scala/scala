object Test extends App {

  def noisyId[A](x: A): x.type = {
    println(s"got $x")
    x
  }

  def testNoConstantFolding(): 23 = {
    println("panda")
    23
  }

  assert(noisyId(42) == 42)
  assert(testNoConstantFolding == 23)
}
