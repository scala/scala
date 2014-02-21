object Test extends App {
  println(new C().blackbox)
  println(new C().refinedBlackbox)
  println(new C().whitebox)
  println(new C().refinedWhitebox)
}