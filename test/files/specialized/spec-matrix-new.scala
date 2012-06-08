import scala.reflect.{ClassTag, classTag}

/** Test matrix multiplication with specialization.
 */

class Matrix[@specialized A: ClassTag](val rows: Int, val cols: Int) {
  private val arr: Array[Array[A]] = Array.ofDim[A](rows, cols)

  def apply(i: Int, j: Int): A = {
    if (i < 0 || i >= rows || j < 0 || j >= cols)
      throw new NoSuchElementException("Indexes out of bounds: " + (i, j))

    arr(i)(j)
  }

  def update(i: Int, j: Int, e: A) {
    arr(i)(j) = e
  }

  def rowsIterator: Iterator[Array[A]] = new Iterator[Array[A]] {
    var idx = 0;
    def hasNext = idx < rows
    def next = {
      idx += 1
      arr(idx - 1)
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val m = randomMatrix(200, 100)
    val n = randomMatrix(100, 200)

    val p = mult(m, n)
    println(p(0, 0))
    println("Boxed doubles: " + runtime.BoxesRunTime.doubleBoxCount)
//    println("Boxed integers: " + runtime.BoxesRunTime.integerBoxCount)
  }

  def randomMatrix(n: Int, m: Int) = {
    val r = new util.Random(10)
    val x = new Matrix[Double](n, m)
    for (i <- 0 until n; j <- 0 until m)
      x(i, j) = (r.nextInt % 1000).toDouble
    x
  }

  def printMatrix[Double](m: Matrix[Double]) {
    for (i <- 0 until m.rows) {
      for (j <- 0 until m.cols)
        print("%5.3f ".format(m(i, j)))
      println
    }
  }

  def multTag[@specialized(Int) T](m: Matrix[T], n: Matrix[T])(implicit at: ClassTag[T], num: Numeric[T]) {
    val p = new Matrix[T](m.rows, n.cols)
    import num._

    for (i <- 0 until m.rows)
      for (j <- 0 until n.cols) {
        var sum = num.zero
        for (k <- 0 until n.rows)
          sum += m(i, k) * n(k, j)
        p(i, j) = sum
      }
  }

  def mult(m: Matrix[Double], n: Matrix[Double]) = {
    val p = new Matrix[Double](m.rows, n.cols)

    for (i <- 0 until m.rows)
      for (j <- 0 until n.cols) {
        var sum = 0.0
        for (k <- 0 until n.rows)
          sum += m(i, k) * n(k, j)
        p(i, j) = sum
      }
    p
  }
}