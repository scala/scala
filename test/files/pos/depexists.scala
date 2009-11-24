object depexists {

  val c: Cell[(a, b)] forSome { type a <: Number; type b <: (a, a) } = null
  val d = c
}
