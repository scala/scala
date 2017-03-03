
trait X { def f: Int }

object Test extends J with X with App {
  println(f)
}

