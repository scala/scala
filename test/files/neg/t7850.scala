// isEmpty returns non-boolean
class Casey(a: Int) { def isEmpty = this; def get = this }
object Casey { def unapply(a: Casey) = a }

// no isEmpty method at all
class Dingy(a: Int) { def get = this }
object Dingy { def unapply(a: Dingy) = a }

object Test {
  def main(args: Array[String]) {
    val Casey(x1) = new Casey(1)
    val Dingy(x2) = new Dingy(1)
    println(s"$x1 $x2")
  }
}

