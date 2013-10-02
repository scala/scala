






trait Bug1[@specialized(Boolean) A] extends TraversableOnce[A] {

  def ++[B >: A](that: TraversableOnce[B]): Iterator[B] = new Iterator[B] {
    lazy val it = that.toIterator
    def hasNext = it.hasNext
    def next = it.next
  }

}



trait WorksFine[@specialized(Boolean) A] {
  class SubBounds[B >: A] extends Bounds[B] {
    lazy val it = ???
  }
  def x[B >: A]: Unit = new SubBounds[B]
}


trait Bounds[@specialized(Boolean) A] {
  // okay without `>: A`
  def x[B >: A]: Unit = new Bounds[B] {
    lazy val it = ???  // def or val okay
  }
}


