import scala.actors.{Future, Futures}

object Test {

  def main(args:Array[String]) : Unit = {
    //scala.actors.Debug.level = 3
    val size = /*if (args.length > 0) Integer.parseInt(args(0)) else*/ 8
    val (m,n) = (size, size)
    def random = (for (i <- 0 until m*n) yield java.lang.Math.random).toArray
    val A = Matrix(m, n, random)
    val B = Matrix(m, n, random)
    val format = new java.text.DecimalFormat("000.00'ms'");
    var iter = 1
    val done = 21
    while (iter < done) {
      val start = System.nanoTime()
      val result = A * B
      val time = System.nanoTime() - start
      result match {
        case Some(result) => {
          printf("     Iteration %2d succeeded %n", iter/*, format.format(time / 1e6)*/)
          iter += 1
        }
        case None => {
          printf(">>>> Iteration %2d failed after %s <<<<< %n", iter, format.format(time / 1e6))
          iter = done
        }
      }
    }
    println("Test done with no deadlock. Try again, it will not occur...")
  }
}

case class Matrix(numRows: Int, numCols: Int, values: Array[Double])  {

  def this(m:Int, n:Int) = this(m, n, new Array[Double](m*n))

  def offset(i:Int, j:Int) = i * numCols + j
  def apply(i:Int, j:Int) = values( offset(i,j) )
  def update(i:Int, j:Int, value:Double) = values(offset(i, j)) = value;

  def *(by:Matrix) = {
    val aM = numRows
    val aN = numCols
    assert(aM == by.numCols)
    assert(aN == by.numRows)
    val resultMatrix = new Matrix(aM, aM)
    val m = aM.asInstanceOf[Int]
    val n = aN.asInstanceOf[Int]

    val rows = for (j <- 0 until m) yield {
      Futures.future {
        try {
        val b_j = new Array[Double](n)
        var k = 0
        while (k < n) { // sadly, while loops are still faster than for loops
          b_j(k) = by(k,j)
          k += 1
        }
        var i = 0
        while (i < m) {
          var s = 0.0d;
          k = 0
          while (k < n) {
            s += Matrix.this(i,k) * b_j(k)
            k += 1
          }
          resultMatrix(i,j) = s
          i += 1
        }
        //printf("future %d of %d completed.%n", j, m)
        j
        } catch {
          case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
            e.printStackTrace()
        }
      }
    }

    // rows.foreach { x=> x() } // This appears to force sequential execution, so use:
    // timeout is 10 years; see http://lampsvn.epfl.ch/trac/scala/ticket/2515
    val done: List[Option[Any]] = try {
      Futures.awaitAll(10*365*24*60*60*1000, rows.toArray : _*) // list to array, as varargs.
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
        List()
    }

    if (done.contains(None))
      None
    else
      Some(resultMatrix)
  }

}
