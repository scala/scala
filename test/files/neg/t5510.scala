trait TypeErrors {
  val sa = s"x\"y  // see t6476.scala
}
trait ParseErrors {
  val sb = "x\"y
  val s1 = s"xxx
  val s2 = s"xxx $x
  val s3 = s"xxx $$
  val s4 = ""s"
  val s5 = ""s""" $s1 $s2 s"
}
