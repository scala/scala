package scala

object RandomAccessSeq {
  trait Projection[+A] extends Seq.Projection[A] with RandomAccessSeq[A] {
    override def projection = this
    protected class MapProjection[B](f : A => B) extends super.MapProjection(f) with Projection[B]
    override def map[B](f: A => B) : Projection[B] = new MapProjection[B](f)
    /**  non-strict */
    override def drop(n: Int): Projection[A] = {
      new Projection[A] {
        def length = Projection.this.length - n
        def apply(idx : Int) = Projection.this.apply(idx + n)
      }
    }
    /**  non-strict */
    override def take(n: Int): Projection[A] = {
      if (n >= length) Projection.this
      else new Projection[A] {
        def length = n
        def apply(idx : Int) = Projection.this.apply(idx)
      }
    }
    /**  non-strict */
    override def slice(from : Int, until : Int) : Projection[A] = drop(from).take(until)
    /**  non-strict */
    override def reverse : Projection[A] = new Projection[A] {
      def length = Projection.this.length
      def apply(idx : Int) = Projection.this.apply(length - idx - 1)
    }
  }
}

/** Sequences that support O(1) element access
 *  @author Sean McDirmid
 */
trait RandomAccessSeq[+A] extends Seq[A] {
  override def projection : RandomAccessSeq.Projection[A] = new RandomAccessSeq.Projection[A] {
    def length = RandomAccessSeq.this.length
    def apply(idx : Int) = RandomAccessSeq.this.apply(idx)
    override def elements = RandomAccessSeq.this.elements
  }
  override def elements : Iterator[A] = new Iterator[A] {
    var idx = 0
    def hasNext = idx < RandomAccessSeq.this.length
    def next = {
      if (!hasNext) throw new Predef.NoSuchElementException
      val ret = RandomAccessSeq.this.apply(idx)
      idx = idx + 1
      ret
    }
  }
  /** Appends two random access sequences in a non-strict way */
  def +++[B >: A](that: RandomAccessSeq[B]): RandomAccessSeq.Projection[B] = new RandomAccessSeq.Projection[B] {
    def length = RandomAccessSeq.this.length + that.length
    def apply(idx : Int) =
      if (idx < RandomAccessSeq.this.length) RandomAccessSeq.this(idx)
      else that(idx - RandomAccessSeq.this.length)
  }
}