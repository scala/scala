package am;

trait One[M[_]] {
  val x : Int
}

trait Two[M[_,_]] {
  val x : Int
}

object Test {
  // Works.
  val x = new Two[Map] {
    val x = 5
  }

  val o = new One[java.util.List] {
    val x = 1
  }

  // Does not work
  val y = new Two[java.util.concurrent.ConcurrentHashMap] {
    val x = 3
  }
}
