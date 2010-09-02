trait X {
  trait A { type T >: Int <: Int }
  val x : A
  var n : x.T = 3
}

trait Y extends X {
  trait B { type T >: String <: String }
  lazy val x : A with B = x
  n = "foo"
}

object Test extends Application {
  new Y {}
}
