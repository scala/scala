import scala.collection.parallel.{ ParMap => PMap }
import scala.collection.parallel.mutable.{ ParHashSet => PMHashSet, ParHashMap => PMHashMap, ParArray }
import scala.util.Random
import scala.collection.parallel.CompositeThrowable

object Test {
  
  def main(args: Array[String]) {
    val N = 1500
    val M = 1500
    var unmatchedLeft = new PMHashSet[Int]
    var unmatchedRight = new PMHashSet[Int]
    Range(0, N).foreach{ x => unmatchedLeft += x}
    Range(0, M).foreach{ x => unmatchedRight += x}
    
    try {
      val matches = unmatchedLeft.flatMap{ lind: Int =>
        val dists = unmatchedRight.seq.map{ rind: Int =>
          val dist = Random.nextInt
          (rind, dist)
        }
        dists
      }
    } catch {
      case c: CompositeThrowable => for (t <- c.throwables) println("\n%s\n%s".format(t, t.getStackTrace.mkString("\n")))
    }
  }
  
}
