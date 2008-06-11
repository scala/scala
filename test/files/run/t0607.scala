object Test extends Application {
  case class A
  case class B extends A
  println(A())
  println(B())
}

