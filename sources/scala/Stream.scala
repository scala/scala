package scala;

trait Stream[a] {

  def isEmpty: Boolean;
  def head: a;
  def tail: Stream[a];

  protected def error[a](x: String):a = (new java.lang.RuntimeException(x)).throw;

  def length: Int = if (isEmpty) 0 else tail.length + 1;

  def append(def rest: Stream[a]): Stream[a] =
    if (isEmpty) rest
    else ConsStream(head, tail.append(rest));

  def init: Stream[a] =
    if (isEmpty) error("EmptyStream.init")
    else if (tail.isEmpty) new EmptyStream[a]()
    else ConsStream(head, tail.init);

  def last: a =
    if (isEmpty) error("EmptyStream.last")
    else if (tail.isEmpty) head
    else tail.last;

  def takeWhile(p: a => Boolean): Stream[a] =
    if (isEmpty || !p(head)) new EmptyStream[a]()
    else ConsStream(head, tail.takeWhile(p));

  def dropWhile(p: a => Boolean): Stream[a] =
    if (isEmpty || !p(head)) this
    else tail.dropWhile(p);

  def take(n: Int): Stream[a] =
    if (n == 0) new EmptyStream[a]()
    else ConsStream(head, tail.take(n-1));

  def drop(n: Int): Stream[a] =
    if (n == 0) this
    else tail.drop(n-1);

  def at(n: Int) = drop(n).head;

  def map[b](f: a => b): Stream[b] =
    if (isEmpty) new EmptyStream[b]()
    else ConsStream(f(head), tail.map(f));

  def foreach(f: a => Unit): Unit =
    if (isEmpty) {}
    else { f(head); tail.foreach(f) }

  def filter(p: a => Boolean): Stream[a] =
    if (isEmpty) this
    else if (p(head)) ConsStream(head, tail.filter(p))
    else tail.filter(p);

  def forall(p: a => Boolean): Boolean =
    isEmpty || (p(head) && tail.forall(p));

  def exists(p: a => Boolean): Boolean =
    !isEmpty && (p(head) || tail.exists(p));

  // the next four functions are obsolete!

  def reduce(op: (a, a) => a): a =
    if (isEmpty) error("reduce of empty stream")
    else tail.fold(op)(head);

  def reduceRight(op: (a, a) => a): a =
    if (isEmpty) error("reduce of empty stream")
    else if (tail.isEmpty) head
    else op(head, tail.reduceRight(op));

  def fold[b](op: (b, a) => b)(z: b): b =
    if (isEmpty) z
    else tail.fold(op)(op(z, head));

  def foldRight[b](op: (a, b) => b)(z: b): b =
    if (isEmpty) z
    else op(head, tail.foldRight(op)(z));

  def flatMap[b](f: a => Stream[b]): Stream[b] =
    if (isEmpty) new EmptyStream[b]()
    else f(head).append(tail.flatMap(f));

  def reverse: Stream[a] = {
    def snoc(xs: Stream[a], x: a): Stream[a] = ConsStream(x, xs);
    fold(snoc)(new EmptyStream[a]())
  }

  // The following method is not compilable without run-time type
  // information. It should therefore be left commented-out for
  // now.
  //       def toArray: Array[a] = {
  //         val xs = new Array[a](length);
  //         copyToArray(xs, 0);
  //         xs
  //       }

  def copyToArray(xs: Array[a], start: Int): Int = {
    xs(start) = head;
    tail.copyToArray(xs, start + 1)
  }

  def zip[b](that: Stream[b]): Stream[[a, b]] =
    if (this.isEmpty || that.isEmpty) new EmptyStream[[a, b]]()
    else ConsStream([this.head, that.head], this.tail.zip(that.tail));

  def print: Unit =
    if (isEmpty) System.out.println("EmptyStream")
    else {
      System.out.print(head as java.lang.Object);
      System.out.print(", ");
      tail.print
    }
}
