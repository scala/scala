object O { class C; @deprecated("", "") type D = C; def foo: Seq[D] = Nil }

object NoWarn {
  O.foo                        // nowarn
  O.foo +: Nil                 // nowarn
  def bar(a: Any, b: Any) = () // nowarn
  bar(b = O.foo, a = ())       // nowarn
}

object Warn {
  type T = O.D
  locally(null: O.D)
  val x: O.D = null
  locally(null.asInstanceOf[O.D])
}
