package scala

object RandomAccessSeq {
  trait Projection[+A] extends Seq.Projection[A] with RandomAccessSeq[A] {
    override def projection = this
    override def force : RandomAccessSeq[A] = toArray
    protected class MapProjection[B](f : A => B) extends super.MapProjection(f) with Projection[B]
    override def map[B](f: A => B) : Projection[B] = new MapProjection[B](f)
    override def append[B >: A](that: => Iterable[B]): Projection[B] = {
      val that0 : Seq[B] = that match {
      case that : Seq.Projection[b] => that
      case that : Seq[b] => that
      case that => that.toList
      }
      new Projection[B] {
        def length = Projection.this.length + that0.length
        def apply(idx : Int) =
          if (idx < Projection.this.length) Projection.this(idx)
          else that0(idx - Projection.this.length)
      }
    }
  }
  /** A random access sequence that supports update (e.g., an array) */
  trait Mutable[A] extends RandomAccessSeq[A] {
    /** <p>
     *    Update the element at given index.
     *  </p>
     *  <p>
     *    Indices start a <code>0</code>; <code>xs.apply(0)</code> is the first
     *    element of mutable sequence <code>xs</code>.
     *  </p>
     *  <p>
     *    Note the indexing syntax <code>xs(i) = x</code> is a shorthand
     *    for <code>xs.update(i, x)</code>.
     *  </p>
     *
     *  @param i   the index
     *  @param x   the value to be written at index <code>i</code>
     *  @throws ArrayIndexOutOfBoundsException if <code>i < 0</code> or
     *          <code>length <= i</code>
     */
    def update(idx : Int, what : A) : Unit
    override def projection : MutableProjection[A] = new MutableProjection[A] {
      override def force : Mutable[A] = Mutable.this
      def update(idx : Int, what : A) : Unit = Mutable.this.update(idx, what)
      def length = Mutable.this.length
      def apply(idx : Int) = Mutable.this.apply(idx)
    }
    def readOnly : RandomAccessSeq[A] = new RandomAccessSeq[A] {
      def length = Mutable.this.length
      def apply(idx : Int) = Mutable.this.apply(idx)
      override def stringPrefix = Mutable.this.stringPrefix + "RO"
    }
    override def drop( from: Int): Mutable[A] = slice(from, length)
    override def take(until: Int): Mutable[A] = slice(0, until)
    override def slice(from : Int, until : Int) : Mutable[A] = {
      if (from == 0 && until >= length) return projection
      else if (from >= until) new MutableProjection[A] {
        def length = 0
        def apply(idx : Int) = throw new Predef.IndexOutOfBoundsException
        def update(idx : Int, what : A) = throw new Predef.IndexOutOfBoundsException
      } else new MutableProjection[A] {
        def length = until - from
        def apply(idx : Int) = if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
                               else Mutable.this.apply(from + idx)
        def update(idx : Int, what : A) =
          if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
          else Mutable.this.update(from + idx, what)
      }
    }
    override def reverse : Mutable[A] = new MutableProjection[A] {
      def update(idx : Int, what : A) : Unit = Mutable.this.update(length - idx - 1, what)
      def length = Mutable.this.length
      def apply(idx : Int) = Mutable.this.apply(length - idx - 1)
      override def stringPrefix = Mutable.this.stringPrefix + "R"
      override def reverse : MutableProjection[A] = Mutable.this.projection
    }
  }
  trait MutableProjection[A] extends Projection[A] with Mutable[A] {
    // XXX: must copy.
    override def force : Mutable[A] = toArray
    override def projection : MutableProjection[A] = this
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
  override def ++[B >: A](that : Iterable[B]) : RandomAccessSeq[B] = that match {
  case that : RandomAccessSeq[b] =>
    val ret = new Array[B](length + that.length)
    copyToArray(ret, 0)
    (that : RandomAccessSeq[B]).copyToArray(ret, length)
    ret
  case that =>
    val buf = new scala.collection.mutable.ArrayBuffer[B]
    this copyToBuffer buf
    that copyToBuffer buf
    buf.readOnly
  }

  override def toStream : Stream[A] = new Stream.Definite[A] {
    override def isEmpty = RandomAccessSeq.this.isEmpty
    override def head = RandomAccessSeq.this.apply(0)
    override def tail = RandomAccessSeq.this.drop(1).toStream
    protected def addDefinedElems(buf: compat.StringBuilder, prefix: String): compat.StringBuilder = {
      var prefix0 = prefix
      var buf0 =buf
      elements.foreach{e =>
        buf0 = buf0.append(prefix0).append(e)
        prefix0 = ", "
      }
      buf0
    }
  }

  /** will return false if index is out of bounds */
  final def safeIs(idx : Int, a : Any) = if (idx >= 0 && idx < length) this(idx) == a else false

}
