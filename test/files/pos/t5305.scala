object t5305 {
  def in(a: Any) = {}

  object O {
    type F = Int
    val v = ""
  }

  in {
    import O.{F, v}
    type x = {type l = (F, v.type)} // not found: type F
  }
}
