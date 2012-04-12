object Test extends App {
  reify {
    class C
    println(classOf[C])
  }.eval
}