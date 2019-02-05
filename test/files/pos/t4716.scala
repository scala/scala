trait Bug2[@specialized(Int) +A] extends IterableOnce[A] {
  def ++[B >: A](that: IterableOnce[B]) = {
    lazy val it = that.iterator
    it
  }
}
