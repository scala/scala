object O {
  val a = 0
}


object test {
  val O1: O.type = O
  val O2: O.type = O
  import O1.a, O2.a
  println(a)
}
