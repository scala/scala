package examples.tcpoly.collection;

trait HOSeq {
  // an internal interface that encapsulates the accumulation of elements (of type elT) to produce
  // a structure of type coll[elT] -- different kinds of collections should provide different implicit
  // values implementing this interface, in order to provide more performant ways of building that structure
  trait Accumulator[+coll[x], elT] {
    def += (el: elT): Unit
    def result: coll[elT]
  }


  // Iterable abstracts over the type of its structure as well as its elements (see PolyP's Bifunctor)
  // m[x] is intentionally unbounded: fold can then be defined nicely
  // variance: if we write m[+x] instead of +m[+x], x is an invariant position because its enclosing type
  //           is an invariant position -- should probably rule that out?
  trait Iterable[+m[+x], +t] {
    //def unit[a](orig: a): m[a]
    def iterator: Iterator[t]

    // construct an empty accumulator that will produce the same structure as this iterable, with elements of type t
    def accumulator[t]: Accumulator[m, t]

    def filter(p: t => Boolean): m[t] = {
      val buf = accumulator[t]
      val elems = iterator
      while (elems.hasNext) { val x = elems.next; if (p(x)) buf += x }
      buf.result
    }

    def map[s](f: t => s): m[s] = {
      val buf = accumulator[s]
      val elems = iterator
      while (elems.hasNext) buf += f(elems.next)
      buf.result
    }

    // flatMap is a more specialized map, it only works if the mapped function produces Iterable values,
    // which are then added to the result one by one
    // the compiler should be able to find the right accumulator (implicit buf) to build the result
    // to get concat, resColl = SingletonIterable, f = unit for SingletonIterable
    def flatMap[resColl[+x] <: Iterable[resColl, x], s](f: t => resColl[s])(implicit buf: Accumulator[resColl, s]): resColl[s] = {
        // TODO:  would a viewbound for resColl[x] be better?
        // -- 2nd-order type params are not yet in scope in view bound
      val elems = iterator
      while (elems.hasNext) {
        val elemss: Iterator[s] = f(elems.next).iterator
        while (elemss.hasNext) buf += elemss.next
      }
      buf.result
    }
  }

  final class ListBuffer[A] {
    private var start: List[A] = Nil
    private var last: ::[A] = _
    private var exported: Boolean = false

    /** Appends a single element to this buffer.
     *
     *  @param x  the element to append.
     */
    def += (x: A) {
      if (exported) copy
      if (start.isEmpty) {
        last = new HOSeq.this.:: (x, Nil)
        start = last
      } else {
        val last1 = last
        last = new HOSeq.this.:: (x, null) // hack: ::'s tail will actually be last
        //last1.tl = last
      }
    }

    /** Converts this buffer to a list
     */
    def toList: List[A] = {
      exported = !start.isEmpty
      start
    }

    /** Clears the buffer contents.
     */
    def clear {
      start = Nil
      exported = false
    }

    /** Copy contents of this buffer */
    private def copy {
      var cursor = start
      val limit = last.tail
      clear
      while (cursor ne limit) {
        this += cursor.head
        cursor = cursor.tail
      }
    }
  }

  implicit def listAccumulator[elT]: Accumulator[List, elT] = new Accumulator[List, elT] {
    private[this] val buff = new ListBuffer[elT]
    def += (el: elT): Unit = buff += el
    def result: List[elT] = buff.toList
  }

  trait List[+t] extends Iterable[List, t] {
    def head: t
    def tail: List[t]
    def isEmpty: Boolean
    def iterator: Iterator[t] = new Iterator[t] {
    var these = List.this
    def hasNext: Boolean = !these.isEmpty
    def next: t =
      if (!hasNext)
        throw new NoSuchElementException("next on empty Iterator")
      else {
        val result = these.head; these = these.tail; result
      }
    }
    // construct an empty accumulator that will produce the same structure as this iterable, with elements of type t
    def accumulator[t]: Accumulator[List, t] = listAccumulator[t]
  }

  // TODO: the var tl approach does not seem to work because subtyping isn't fully working yet
  final case class ::[+b](hd: b, private val tl: List[b]) extends List[b] {
    def head = hd
    def tail = if(tl==null) this else tl // hack
    override def isEmpty: Boolean = false
  }

  case object Nil extends List[Nothing] {
    def isEmpty = true
    def head: Nothing =
      throw new NoSuchElementException("head of empty list")
    def tail: List[Nothing] =
      throw new NoSuchElementException("tail of empty list")
  }
}



// misc signatures collected from mailing list / library code:
    /*override def flatMap[B](f: A => Iterable[B]): Set[B]
    final override def flatMap[b](f: Any => Iterable[b]): Array[b]
     def flatMap[b](f: a => Parser[b]) = new Parser[b]
     override def flatMap[b](f: a => Iterable[b]): List[b]


    MapResult[K] <: Seq[K]
    FilterResult <: Seq[T]
    Concat <: Seq[T]
    Subseq <: Seq[T]


    def map[K](f: T=>K): MapResult[K]
    def filter(f: T=>Boolean): FilterResult
    def subseq(from: Int, to: Int): Subseq
    def flatMap[S <: Seq[K], K](f: T => S): S#Concat  // legal?
    def concat(others: Seq[T]): Concat
     */

/*trait Iterator[t] {
  // @post hasAdvanced implies hasNext
  // model def hasAdvanced: Boolean

  def hasNext: Boolean // pure

  // @pre hasAdvanced
  def current: t       // pure

  // @pre hasNext
  // @post hasAdvanced
  def advance: Unit
}*/
