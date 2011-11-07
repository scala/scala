package scala.collection.parallel.benchmarks.parallel_array



import collection.parallel.immutable.ParRange


object MatrixMultiplication extends Companion {
  def benchName = "matrix-mult";
  def apply(sz: Int, parallelism: Int, what: String) = new MatrixMultiplication(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 100
}

class MatrixMultiplication(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = MatrixMultiplication
  collection.parallel.tasksupport.environment = forkjoinpool
  
  val a = Matrix.unit[Int](sz)
  val b = Matrix.unit[Int](sz)
  var c = new Matrix[Int](sz)
  
  def runpar = c = a * b //{ c.assignProduct(a, b) } //; println("--------"); c.output }
  def runseq = throw new UnsupportedOperationException
  def comparisonMap = collection.Map()
  
  class Matrix[T](n: Int)(implicit num: Numeric[T], man: Manifest[T]) {
    val array = new Array[T](n * n)
    
    def apply(y: Int, x: Int) = array(y * n + x)
    
    def update(y: Int, x: Int, elem: T) = array(y * n + x) = elem
    
    def *(b: Matrix[T]) = {
      val m = new Matrix[T](n)
      m.assignProduct(this, b)
      m
    }
    
    def assignProduct(a: Matrix[T], b: Matrix[T]) = {
      val range = ParRange(0, n * n, 1, false)
      for (i <- range) this(i / n, i % n) = calcProduct(a, b, i / n, i % n);
    }
    
    private def calcProduct(a: Matrix[T], b: Matrix[T], y: Int, x: Int): T = {
      import num._
      var sum = zero
      for (i <- 0 until n) sum += a(y, i) * b(i, x)
      sum
    }
    
    def output = for (y <- 0 until n) {
      for (x <- 0 until n) print(this(y, x))
      println
    }
  }
  
  object Matrix {
    def unit[T](n: Int)(implicit num: Numeric[T], man: Manifest[T]) = {
      val m = new Matrix[T](n)
      for (i <- 0 until n) m(i, i) = num.one
      m
    }
  }
  
}

















