trait C1[+A, +CC[X]] {
  protected[this] def f: A => CC[A] = sys.error("")
}

trait C2[+A, +CC[X]] extends C1[A, CC] {
  override protected[this] def f = super.f
}