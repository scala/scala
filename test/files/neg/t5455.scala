trait Test {
  def root: Test

  @annotation.tailrec final lazy val bar: Thing[Int] = {
    if (this eq root)
      Thing(() => System.identityHashCode(bar))
    else
      root.bar
  }

  def f = bar.f()
}

case class Thing[A](f: () => A) {
  override def toString = "" + f()
}
