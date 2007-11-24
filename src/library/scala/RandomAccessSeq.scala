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
  private[scala] trait Slice[+A] extends Projection[A] {
    protected def from  : Int
    protected def until : Int
    protected def underlying : RandomAccessSeq[A]
    def length = {
      val length0 = underlying.length
      if (from >= until || from >= length0) 0
      else (if (until >= length0) length0 else until) - (if (from < 0) 0 else from)
    }
    def apply(idx : Int) = if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
                           else underlying.apply((if (from < 0) 0 else from) + idx)
    override def slice(from0 : Int, until0 : Int) = {
      val from = if (this.from < 0) 0 else this.from
      underlying.slice(from + from0, from + until0)
    }
  }
  private[scala] trait Patch[+A] extends Projection[A] {
    protected def original : RandomAccessSeq[A]
    protected def patch : RandomAccessSeq[A]
    protected def from : Int
    protected def replaced : Int
    def length = original.length + patch.length - replaced
    def apply(idx : Int) =
      if (idx < from) original.apply(idx)
      else if (idx < from + patch.length) patch.apply(idx - from)
      else original.apply(idx - patch.length + replaced)
    override def stringPrefix = "patch"
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
    override def drop( from: Int): MutableProjection[A] = slice(from, length)
    override def take(until: Int): MutableProjection[A] = slice(0, until)
    override def slice(from0 : Int, until0 : Int) : MutableProjection[A] = new MutableSlice[A] {
      def from = from0
      def until = until0
      def underlying = Mutable.this
    }
    override def reverse : MutableProjection[A] = new MutableProjection[A] {
      def update(idx : Int, what : A) : Unit = Mutable.this.update(length - idx - 1, what)
      def length = Mutable.this.length
      def apply(idx : Int) = Mutable.this.apply(length - idx - 1)
      override def stringPrefix = Mutable.this.stringPrefix + "R"
      override def reverse : MutableProjection[A] = Mutable.this.projection
    }
  }
  trait MutableProjection[A] extends Projection[A] with Mutable[A] {
    override def force : Mutable[A] = toArray
    override def projection : MutableProjection[A] = this
  }
  private[scala] trait MutableSlice[A] extends MutableProjection[A] with Slice[A] {
    protected def underlying : Mutable[A]
    override def slice(from0 : Int, until0 : Int) = {
      val from = (if (this.from < 0) 0 else this.from)
      underlying.slice(from + from0, from + until0)
    }
    override def update(idx : Int, what : A) : Unit = {
      val from = (if (this.from < 0) 0 else this.from)
      if (idx < 0 || idx >= length) throw new Predef.IndexOutOfBoundsException
      else underlying.update(from + idx, what)
    }
  }
}

/** Sequences that support O(1) element access and O(1) length computation.
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
  override def slice(from0 : Int, until0 : Int) : RandomAccessSeq[A] = new RandomAccessSeq.Slice[A] {
    def from = from0
    def until = until0
    def underlying = RandomAccessSeq.this
  }
  override def reverse : Seq[A] = new RandomAccessSeq.Projection[A] {
    def length = RandomAccessSeq.this.length
    def apply(idx : Int) = RandomAccessSeq.this.apply(length - idx - 1)
    override def stringPrefix = RandomAccessSeq.this.stringPrefix + "R"
    override def reverse : RandomAccessSeq.Projection[A] = RandomAccessSeq.this.projection
  }

  /** insert segment <code>patch</code> into this sequence at <code>from</code>
   *  replacing  <code>replaced</code> elements. The result is a projection.
   */
  def patch[B >: A](from0 : Int, patch0 : RandomAccessSeq[B], replaced0 : Int) : RandomAccessSeq.Projection[B] = new RandomAccessSeq.Patch[B] {
    override def original = RandomAccessSeq.this
    override def from = from0
    override def patch = patch0
    override def replaced = replaced0
    override def stringPrefix = RandomAccessSeq.this.stringPrefix + "P"
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
