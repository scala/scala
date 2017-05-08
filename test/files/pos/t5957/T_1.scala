abstract class T {
  // see: scala/bug#6109
  // def t1: Test$Bar
  def t2: Test#Bar
  // see: scala/bug#6109
  // def t3: Test$Baz
  def t4: Test.Baz
}
