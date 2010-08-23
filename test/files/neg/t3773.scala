object t {
  val m = Map(1 -> "one")
  for ((v, t) <- m.elements) ()
}

