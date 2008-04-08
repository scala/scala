object t0732 extends Application {
  val x1 = <a b="c" />
  println(x1 \ "@b")
  val x2 = <a x:b="c" xmlns:x="uri" />
  println(x2 \ "@b")
  val x3 = <a x:b="c" xmlns:x="uri" />
  println(x3 \ "@x:b")
}
