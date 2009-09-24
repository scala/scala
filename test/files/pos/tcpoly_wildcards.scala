trait test[b[_,_]] {
  def moo[a[_, _]] = error("a")
}
