object Test extends App {
  def testSeq(name: String, expected: Seq[Char], got: Seq[Char]) {
    if (expected.toList == got.toList)
      println(name + " ok")
    else
      println(name + " failed: " + expected + " differs from " + got)
  }
  
  testSeq("'a' to 'c'", List('a', 'b', 'c'), 'a' to 'c')
  testSeq("'a' until 'c'", List('a', 'b'), 'a' until 'c')
  
  testSeq("'a' to 'b'", List('a', 'b'), 'a' to 'b')
  testSeq("'a' until 'b'", List('a'), 'a' until 'b')
  
  testSeq("'a' to 'a'", List('a'), 'a' to 'a')
  testSeq("'a' until 'a'", List(), 'a' until 'a')
  
  testSeq("'b' to 'a'", List(), 'b' to 'a')
  testSeq("'b' until 'a'", List(), 'b' until 'a')
  
  testSeq("'c' to 'a'", List(), 'c' to 'a')
  testSeq("'c' until 'a'", List(), 'c' until 'a')
}

// vim: set ts=2 sw=2 et:
