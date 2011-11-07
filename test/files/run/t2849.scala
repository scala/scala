import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet

object Test {

  def main(args: Array[String]) {
    ticketExample
    similarExample
  }

  //def freeMBytes = Runtime.getRuntime.freeMemory / 1048576
  def totalMBytes = Runtime.getRuntime.totalMemory / 1048576

  val N = if (totalMBytes > 1000) 10000 else 4000
  val M = N / 10

  def ticketExample {
    var big = 100000
    
    var aSortedSet: SortedSet[Int] = TreeSet(big)
    
    for (i <- 1 until N) {
      aSortedSet = (aSortedSet - big) ++ (TreeSet(i, big - 1))
      big -= 1
      if (i % M == 0) {
        //println("big: "+big+", free memory: "+freeMBytes)
        aSortedSet.until(i)
      }
    }
  }

  def similarExample {
    var big = 100

    var aSortedSet: SortedSet[Int] = TreeSet(big)

    for (i <- 1 until N) {
      aSortedSet = (aSortedSet - big) ++ (TreeSet(i, big - 1)) + big
      big -= 1
      if (i % M == 0) {
        aSortedSet.until(i)
      }
    }
  }
  
}


