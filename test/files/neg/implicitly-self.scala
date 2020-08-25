// scalac: -Xfatal-warnings -Ywarn-self-implicit

trait TC[T] { def ix: Int }

object Test {
  implicit def c: Char = implicitly[Char]
  implicit val s: String = implicitly[String]
  implicit val t: Int = {
    def f = implicitly[Int]
    f
  }
  implicit object tcString extends TC[String] { def ix = implicitly[TC[String]].ix + 1 }
}
