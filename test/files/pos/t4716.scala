



trait Bug2[@specialized(Int) +A] extends TraversableOnce[A] {  
  def ++[B >: A](that: TraversableOnce[B]) = {
    lazy val it = that.toIterator
    it
  }
}
