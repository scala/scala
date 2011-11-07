/** Test matrix multiplication with specialization.
 */

class Matrix[@specialized A: ClassManifest](val rows: Int, val cols: Int) {
  private val arr: Array[Array[A]] = new Array[Array[A]](rows, cols)
  
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

    mult(m, n)
    println("*")
  }

  def randomMatrix(n: Int, m: Int) = {
    val r = new util.Random(10)
    val x = new Matrix[Int](n, m) 
    for (i <- 0 until n; j <- 0 until m)
      x(i, j) = r.nextInt
    x
  }


  def multManifest[@specialized(Int) T](m: Matrix[T], n: Matrix[T])(implicit cm: ClassManifest[T], num: Numeric[T]) {
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

  def mult(m: Matrix[Int], n: Matrix[Int]) {
    val p = new Matrix[Int](m.rows, n.cols)

    for (i <- 0 until m.rows) 
      for (j <- 0 until n.cols) {
        var sum = 0
        for (k <- 0 until n.rows)
          sum += m(i, k) * n(k, j)
        p(i, j) = sum
      }
  }
}
