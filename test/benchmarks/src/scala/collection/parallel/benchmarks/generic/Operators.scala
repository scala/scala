package scala.collection.parallel.benchmarks.generic






trait Operators[T] {
  
  def foreachFun: T => Unit
  def reducer: (T, T) => T
  def mediumreducer: (T, T) => T
  def filterer: T => Boolean
  def mapper: T => T
  def mapper2: T => T = error("unsupported")
  def heavymapper: T => T
  def flatmapper: T => Seq[T]
  def taker: T => Boolean
  def eachFun: T => Unit
  def eachPairFun: ((T, T)) => Unit = error("unsupported")
  def sequence(sz: Int): Seq[T] = error("unsupported")
  
}



trait IntOperators extends Operators[Int] {
  
  val foreachFun: Int => Unit = x => ()
  val reducer: (Int, Int) => Int = _ + _
  val mediumreducer: (Int, Int) => Int = (a: Int, b: Int) => {
    val result = if (b == 0) a else {
      mediumreducer.apply(b, a - b * (a / b))
    }
    result + 1000
  }
  val filterer: Int => Boolean = _ % 2 == 0
  val mapper: Int => Int = _ * 2
  val heavymapper: Int => Int = (n: Int) => {
    var i = -10
    var sum = 0
    while (i < 0) {
      sum += -i
      i += 1
    }
    n + sum
  }
  val flatmapper: Int => Seq[Int] = (n: Int) => {
    List(n, n, n, n, n)
  }
  val taker: Int => Boolean = _ < 10000
  val eachFun: Int => Unit = { n =>
    n % 2 == 0
  }
  
}








