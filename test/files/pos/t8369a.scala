object Bug {
  trait Sys[S]
  def test[S <: Sys[S]] = read[S]()
  def read[S <: Sys[S]](baz: Any = 0): Some[S] = ???
}