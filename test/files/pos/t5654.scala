class T(val a: Array[_])

class U {
  val a = Array(Array(1, 2), Array("a","b"))
}

class T1 { val a: Array[_] = Array(1) }

case class Bomb(a: Array[_])
case class Bomb2(a: Array[T] forSome { type T })
class Okay1(a: Array[_])
case class Okay2(s: Seq[_])

