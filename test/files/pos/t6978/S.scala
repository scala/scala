//> using options -Xlint -Xfatal-warnings
trait X { def f: Int }

object Test extends J with X with App {
  println(f)
}

