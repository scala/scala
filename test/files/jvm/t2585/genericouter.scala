case class S(n:Int)

trait TraversableLike[+A, +Repr] {
  class WithFilter(p: A => Boolean)
  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)
}

class HashMap[K, +V] extends TraversableLike[(K, V), HashMap[K, V]]

class Outer[T](val t: T) {
  class Inner {
    def getT : T = t
  }
}

class OuterImpl(x: X) extends Outer[X](x) {
  def newInner = new Inner
}

class X {
  def getI : Outer[X]#Inner = {
    val oImpl = new OuterImpl(this)
    new oImpl.Inner
  }
}