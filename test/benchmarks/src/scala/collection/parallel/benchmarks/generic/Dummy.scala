package scala.collection.parallel.benchmarks.generic




class Dummy(val in: Int) {
  var num = in
  override def toString = in.toString
  override def hashCode = in
  def dummy = num + in
  def one = "1".length
}


object DummyOperators extends Operators[Dummy] {
  val foreachFun = (a: Dummy) => {
    a
    ()
  }
  val reducer = (a: Dummy, b: Dummy) => {
    var i = 0
    if (a.in > b.in) {
      a.num = a.in + b.in + i
      a
    } else {
      b.num = a.in + b.in + i
      b
    }
  }
  private def rec(a: Int, b: Int): Int = {
    val result = if (b == 0) a else {
      rec(b, a - b * (a / b))
    }
    result + 1000
  }
  val mediumreducer = (a: Dummy, b: Dummy) => {
    var i = 0
    var sum = a.num + b.num
    b.num = rec(a.num, b.num)
    b
  }
  val filterer = (a: Dummy) => {
    a.in % 2 == 0
  }
  val mapper = (a: Dummy) => {
    a.num = a.dummy + a.num + a.in + a.one
    a
  }
  override val mapper2 = (a: Dummy) => {
    val x = 1
    new Dummy(a.in * -2 + x)
  }
  val heavymapper = (a: Dummy) => {
    var i = -100
    while (i < 0) {
      if (a.in < i) a.num += 1
      i += 1
    }
    a
  }
  val flatmapper = (a: Dummy) => {
    List(a, a, a, a, a)
  }
  val taker = (a: Dummy) => {
    a.in >= 0
  }
  val eachFun: Dummy => Unit = (d: Dummy) => {
    d.dummy
  }
  override val eachPairFun: ((Dummy, Dummy)) => Unit = p => {
    p._1.dummy
    p._2.dummy
  }
  override def sequence(sz: Int): Seq[Dummy] = {
    val pa = new collection.parallel.mutable.ParArray[Dummy](sz)
    for (i <- 0 until sz) pa(i) = new Dummy(i)
    pa
  }
}




