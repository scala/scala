case class Bomb(a: Array[_])
case class Bomb2(a: Array[T] forSome { type T })
class Okay1(a: Array[_])
case class Okay2(s: Seq[_])