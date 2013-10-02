import collection.immutable.HashMap


object Test {

  def main(args: Array[String]) {
    resolveDefault()
    resolveFirst()
    resolveSecond()
    resolveMany()
  }

  def resolveDefault() {
    val a = HashMap(1 -> "1")
    val b = HashMap(1 -> "2")

    val r = a.merged(b)(null)
    println(r)
    println(r(1))
  }

  def resolveFirst() {
    val a = HashMap(1 -> "1")
    val b = HashMap(1 -> "2")
    def collision(a: (Int, String), b: (Int, String)) = {
      println(a)
      a
    }

    val r = a.merged(b) { collision }
    println(r)
    println(r(1))
  }

  def resolveSecond() {
    val a = HashMap(1 -> "1")
    val b = HashMap(1 -> "2")
    def collision(a: (Int, String), b: (Int, String)) = {
      println(b)
      b
    }

    val r = a.merged(b) { collision }
    println(r)
    println(r(1))
  }

  def resolveMany() {
    val a = HashMap((0 until 100) zip (0 until 100): _*)
    val b = HashMap((0 until 100) zip (100 until 200): _*)
    def collision(a: (Int, Int), b: (Int, Int)) = {
      (a._1, a._2 + b._2)
    }

    val r = a.merged(b) { collision }
    for ((k, v) <- r) assert(v == 100 + 2 * k, (k, v))
  }

}
