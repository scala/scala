// /scala/trac/4123/a.scala
// Sun Feb 19 00:08:53 PST 2012

trait Iter[@specialized(Byte) +A] extends Iterator[A] {
  self =>
  
  override def map[B](f: (A) => B) = super.map(f)
}

class ByteIter extends Iter[Byte] {
  var i = 0
  def hasNext = i < 3
  def next = { i += 1 ; i.toByte }
}