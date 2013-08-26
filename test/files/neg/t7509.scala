object NMWE {
  def crash[R <: AnyRef](f: R)(implicit ev: R): Any = ???
  crash(42)
}
