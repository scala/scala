package scala;

trait Stream[+a] {

  def isEmpty: Boolean;
  def head: a;
  def tail: Stream[a];

  def length: Int = if (isEmpty) 0 else tail.length + 1;

  def append(def rest: Stream[a]): Stream[a] =
    if (isEmpty) rest
    else Stream.cons(head, tail.append(rest));

  def init: Stream[a] =
    if (isEmpty) error("Stream.empty.init")
    else if (tail.isEmpty) Stream.empty[a]
    else Stream.cons(head, tail.init);

  def last: a =
    if (isEmpty) error("Stream.empty.last")
    else if (tail.isEmpty) head
    else tail.last;

  def takeWhile(p: a => Boolean): Stream[a] =
    if (isEmpty || !p(head)) Stream.empty[a]
    else Stream.cons(head, tail.takeWhile(p));

  def dropWhile(p: a => Boolean): Stream[a] =
    if (isEmpty || !p(head)) this
    else tail.dropWhile(p);

  def take(n: Int): Stream[a] =
    if (n == 0) Stream.empty[a]
    else Stream.cons(head, tail.take(n-1));

  def drop(n: Int): Stream[a] =
    if (n == 0) this
    else tail.drop(n-1);

  def at(n: Int) = drop(n).head;

  def map[b](f: a => b): Stream[b] =
    if (isEmpty) Stream.empty[b]
    else Stream.cons(f(head), tail.map(f));

  def foreach(f: a => Unit): Unit =
    if (isEmpty) {}
    else { f(head); tail.foreach(f) }

  def filter(p: a => Boolean): Stream[a] =
    if (isEmpty) this
    else if (p(head)) Stream.cons(head, tail.filter(p))
    else tail.filter(p);

  def forall(p: a => Boolean): Boolean =
    isEmpty || (p(head) && tail.forall(p));

  def exists(p: a => Boolean): Boolean =
    !isEmpty && (p(head) || tail.exists(p));

  def foldLeft[b](z: b)(f: (b, a) => b): b =
    if (isEmpty) z
    else tail.foldLeft[b](f(z, head))(f);

  def foldRight[b](z: b)(f: (a, b) => b): b =
    if (isEmpty) z
    else f(head, tail.foldRight(z)(f));

  def /:[b](z: b)(f: (b, a) => b): b = foldLeft(z)(f);
  def :/[b](z: b)(f: (a, b) => b): b = foldRight(z)(f);

  def reduceLeft[b >: a](f: (b, b) => b): b =
    if (isEmpty) error("Stream.empty.reduceLeft")
    else ((tail: Stream[b]) foldLeft (head: b))(f);

  def reduceRight[b >: a](f: (b, b) => b): b =
    if (isEmpty) error("Stream.empty.reduceRight")
    else if (tail.isEmpty) head: b
    else f(head, tail.reduceRight(f));

  def flatMap[b](f: a => Stream[b]): Stream[b] =
    if (isEmpty) Stream.empty[b]
    else f(head).append(tail.flatMap(f));

  def reverse: Stream[a] = {
    def snoc(xs: Stream[a], x: a): Stream[a] = Stream.cons(x, xs);
    foldLeft(Stream.empty[a])(snoc)
  }

  // The following method is not compilable without run-time type
  // information. It should therefore be left commented-out for
  // now.
  //       def toArray: Array[a] = {
  //         val xs = new Array[a](length);
  //         copyToArray(xs, 0);
  //         xs
  //       }

  def copyToArray[b >: a](xs: Array[b], start: Int): Int = {
    xs(start) = head;
    tail.copyToArray(xs, start + 1)
  }

  def zip[b](that: Stream[b]): Stream[Tuple2[a, b]] =
    if (this.isEmpty || that.isEmpty) Stream.empty[Tuple2[a, b]]
    else Stream.cons(Tuple2(this.head, that.head), this.tail.zip(that.tail));

  def print: Unit =
    if (isEmpty) System.out.println("Stream.empty")
    else {
      System.out.print(head as java.lang.Object);
      System.out.print(", ");
      tail.print
    }
}

object Stream {

  def empty[c]: Stream[c] = new Stream[c] {
    def isEmpty = true;
    def head: c = error("head of empty stream");
    def tail: Stream[c] = error("tail of empty stream");
    override def toString(): String = "Stream.empty";
  }

  def cons[b](hd: b, def tl: Stream[b]): Stream[b] = new Stream[b] {
    def isEmpty = false;
    def head = hd;
    private var tlVal: Stream[b] = _;
    private var tlDefined: Boolean = false;
    def tail: Stream[b] = {
      if (!tlDefined) { tlVal = tl; tlDefined = true; }
      tlVal
    }
    override def toString(): String = "ConsStream(" + hd + ", ?)";
  }

  def concat[a](xs: Seq[Stream[a]]): Stream[a] = concat(xs.elements);

  def concat[a](xs: Iterator[Stream[a]]): Stream[a] = {
    if (xs.hasNext) xs.next append concat(xs)
    else empty;
  }
}
