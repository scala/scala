class A

case object B extends A

object Test {
  val x1 = (B: A)

  println(x1 == B) // no warning
  println(B == x1) // no warning

  val x2 = (B: A with Product)

  println(x2 == B) // no warning
  println(B == x2) // spurious warning: "always returns false"
}
