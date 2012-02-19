trait Bug1[@specialized +A] extends TraversableOnce[A] {  
  def ++[B >: A](that: TraversableOnce[B]): Iterator[B] = new Iterator[B] {
    lazy val it = that.toIterator
    def hasNext = it.hasNext
    def next = it.next
  }
}