object Test {
  val sa = s"x\"y
  val sb = "x\"y
  val s1 = s"xxx
  val s2 = s"xxx $x
  val s3 = s"xxx $$
  val s4 = ""s"
  val s5 = ""s""" $s1 $s2 s"
}
