trait LazyExp[+This <: LazyExp[This]] { this: This =>
  type OpSemExp <: LazyExp[OpSemExp] with This
  type Val
}

object Test {
  def foo[AA <: LazyExp[_]](a: AA): a.OpSemExp#Val = ??? // a.OpSemExp is volatile, because of `with This`
}
