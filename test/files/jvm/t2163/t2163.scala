class T2163Scala[CC[X]](x: CC[Int]) {
  def bar[DD[X]](meh: DD[Int]): CC[Int] = x
}

object Test extends App {}
