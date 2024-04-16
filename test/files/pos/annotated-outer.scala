//> using options -Werror
object Test {
  trait MySet[A]
  trait MyMap[K, +V] {
    class Keys extends MySet[K]
  }

  def areKeys[A](xs: MySet[A]) = xs match {
    case _: (MyMap[A, _] @unchecked)#Keys => true
    case _ => false
  }
}
