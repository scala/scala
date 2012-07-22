abstract class T {
  // see: SI-6109
  // def t1: Test$Bar
  def t2: Test#Bar
  // see: SI-6109
  // def t3: Test$Baz
  def t4: Test.Baz
}
