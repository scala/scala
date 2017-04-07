object Test {

  def unfold[A, B](a: A)(f: A => Option[(B, A)]): Stream[B] = f(a) match {
    case Some((b, a)) => b #:: unfold(a)(f)
    case None         => Stream.empty
  }

  case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {
    def next = right match {
      case h +: rest => Some(StreamZipper(focus +: left, h, rest))
      case _         => None

    }
    def previous = left match {
      case h +: rest => Some(StreamZipper(rest, h, focus +: right))
      case _         => None

    }
    def map[B](f: A => B) = StreamZipper(left map f, f(focus), right map f)
    def extract = focus
    def duplicate: StreamZipper[StreamZipper[A]] = {
      val r = unfold(this)(z => z.next.map(x => (x, x)))
      val l = unfold(this)(z => z.previous.map(x => (x, x)))
      StreamZipper(l, this, r)
    }
    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B] = duplicate map f

    def toStream = left.reverse ++ (focus +: right)
    
  }
  
  def stream2zipper[A](sa: Stream[A]) = sa match {
    case h +: rest => Some(StreamZipper(Stream.empty, h, rest))
    case _ => None
  }
  
  
  case class Person(name: String, age: Int)
  
  
  val people = Stream(Person("john", 40),Person("alice", 45),Person("jones", 50),Person("bob", 60),Person("jane", 50))
  
  
  val Some(zi) = stream2zipper(people)
  def isSandwitch(z : StreamZipper[Int]) = z match {
    case StreamZipper(pi +: _, i, ni +: _) if (pi < i && i < ni) => true
    case _ => false
  }
  
  def fff(z : StreamZipper[Person]) = z.extract
  def numberOfTruth(z: StreamZipper[Boolean]) : Int  = 
    if (z.focus) 1 + z.right.takeWhile(true ==).size else 0
  
  val result : StreamZipper[Person] => (Person, Boolean, Int) = cofor (p @ Person(_, age)) {
    tag <- isSandwitch(age)
    count <- numberOfTruth(tag)
  } yield (p.extract, tag.extract, count.extract)
  
  
  zi.coflatMap(result).toStream
  
  val result2 : StreamZipper[Person] => (Person, Boolean, Int) = cofor (p) {
    Person(_, age) <- fff(p)
    tag <- isSandwitch(age)
    count <- numberOfTruth(tag)
  } yield (p.extract, tag.extract, count.extract)
  
  
  zi.coflatMap(result2).toStream
}
