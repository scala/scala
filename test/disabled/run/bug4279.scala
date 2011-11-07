import scala.tools.partest._

// Attempting to verify slice isn't 100,000x slower
// with views than non-views.
class Runner(num: Int, reps: Int) extends TestUtil {
  var dummy = 0
  val range = Array.range(0, num)
  
  def iteratorSlice = {
    def it = range.iterator.slice(num - 2, num)
    for (i <- 1 to reps)
      it foreach (dummy = _)
  }
  def viewSlice = {
    val view = range.view.slice(num - 2, num)
    for (i <- 1 to reps)
      view foreach (dummy = _)
  }
  def straightSlice = {
    val xs = range.slice(num - 2, num)
    for (i <- 1 to reps)
      xs foreach (dummy = _)
  }
  def run(multiple: Double) = {
    verifySpeed(straightSlice, iteratorSlice, multiple)
    verifySpeed(straightSlice, viewSlice, multiple)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    // warmup
    { val r = new Runner(1000000, 10) ; r.straightSlice ; r.iteratorSlice ; r.viewSlice }

    new Runner(10000000, 10) run 500
    new Runner(10000000, 50) run 300
  }
}
