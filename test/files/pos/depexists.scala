object depexists {

  val c: Option[(a, b)] forSome { type a <: Number; type b <: (a, a) } = null
  val d = c
}
