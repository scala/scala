
object Test {

  def main(args: Array[String]) = {
     val one = "1"
     val two = "2"

     val raw = raw"\n$one\n$two\n"
     val escaped = s"\n$one\n$two\n"
     val buggy = "\\n1\n2\n"
     val correct = "\\n1\\n2\\n"

     assert(raw != escaped, "Raw strings should not be escaped.")
     assert(raw != buggy, "Raw strings after variables should not be escaped.")
     assert(raw == correct, "Raw strings should stay raw.")
  }
}
