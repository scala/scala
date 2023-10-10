
//> using options -Ylegacy-symbol-pos

/* some header cruft */

class C {
  def p() = println("hello, world")
}

object Test extends App {
  val Some((start, end, text)) = denamer.span[C]
  assert(end - start > 5)
  assert(text.startsWith("class C "))
}
