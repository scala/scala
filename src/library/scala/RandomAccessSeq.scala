package scala

object RandomAccessSeq {
  trait Projection[+A] extends Seq.Projection[A] with RandomAccessSeq[A] {
    override def projection = this
    protected class MapProjection[B](f : A => B) extends super.MapProjection(f) with Projection[B]
    override def map[B](f: A => B) : Projection[B] = new MapProjection[B](f)

  }
  /** A random access sequence that supports update (e.g., an array) */
  trait Mutable[A] extends RandomAccessSeq[A] {
    def update(idx : Int, what : A) : Unit
    override def projection : MutableProjection[A] = new MutableProjection[A] {
      def update(idx : Int, what : A) : Unit = Mutable.this.update(idx, what)
      def length = Mutable.this.length
      def apply(idx : Int) = Mutable.this.apply(idx)
    }
    def readOnly : RandomAccessSeq[A] = new RandomAccessSeq[A] {
      def length = Mutable.this.length
      def apply(idx : Int) = Mutable.this.apply(idx)
      override def stringPrefix = Mutable.this.stringPrefix + "RO"
    }
  }
  trait MutableProjection[A] extends Projection[A] with Mutable[A] {
    override def projection : MutableProjection[A] = this
    override def reverse : MutableProjection[A] = new MutableProjection[A] {
      def update(idx : Int, what : A) : Unit = MutableProjection.this.update(length - idx - 1, what)
      def length = MutableProjection.this.length
      def apply(idx : Int) = MutableProjection.this.apply(length - idx - 1)
      override def stringPrefix = MutableProjection.this.stringPrefix + "R"
      override def reverse : MutableProjection[A] = MutableProjection.this
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
    override def stringPrefix = RandomAccessSeq.this.stringPrefix + "P"
  }
  override def elements : Iterator[A] = new BufferedIterator.Advanced[A] {
    var idx = 0
    override def peekList(sz : Int) = new RandomAccessSeq[A] {
      override def length = RandomAccessSeq.this.length - idx
      override def apply(jdx : Int) = RandomAccessSeq.this.apply(jdx + idx)
    }
    override def hasNext = idx < RandomAccessSeq.this.length
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
  override def drop( from: Int): RandomAccessSeq[A] = slice(from, length)
  override def take(until: Int): RandomAccessSeq[A] = slice(0, until)
  override def slice(from : Int, until : Int) : RandomAccessSeq[A] = {
    if (from == 0 && until >= length) return projection
    else if (from >= until) new RandomAccessSeq.Projection[A] {
      def length = 0
      def apply(idx : Int) = throw new Predef.IndexOutOfBoundsException
    } else new RandomAccessSeq.Projection[A] {
      def length = until - from
      def apply(idx : Int) = if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
                             else RandomAccessSeq.this.apply(from + idx)
    }
  }

  override def reverse : Seq[A] = new RandomAccessSeq.Projection[A] {
    def length = RandomAccessSeq.this.length
    def apply(idx : Int) = RandomAccessSeq.this.apply(length - idx - 1)
    override def stringPrefix = RandomAccessSeq.this.stringPrefix + "R"
    override def reverse : RandomAccessSeq.Projection[A] = RandomAccessSeq.this.projection
  }
  /** will return false if index is out of bounds */
  final def safeIs(idx : Int, a : Any) = if (idx >= 0 && idx < length) this(idx) == a else false

}
