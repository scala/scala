trait test[b[_,_]] {
  def moo[a[_, _]] = sys.error("a")
}
