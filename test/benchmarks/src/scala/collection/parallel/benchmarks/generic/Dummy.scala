package scala.collection.parallel.benchmarks.generic




class Dummy(val in: Int) {
  var num = in
  override def toString = in.toString
}


object DummyOperators extends Operators[Dummy] {
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
    a.num = a.in % 2
    a
  }
  val heavymapper = (a: Dummy) => {
    var i = -100
    while (i < 0) {
      if (a.in < i) a.num += 1
      i += 1
    }
    a
  }
  val taker = (a: Dummy) => {
    a.in >= 0
  }
}





