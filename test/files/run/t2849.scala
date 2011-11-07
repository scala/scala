


import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet



object Test {
  
  def main(args: Array[String]) {
    ticketExample
    similarExample
  }
  
  def ticketExample {
    var big = 100000
    
    var aSortedSet: SortedSet[Int] = TreeSet(big)
    
    for (i <- 1 until 10000) {
      aSortedSet = (aSortedSet - big) ++ (TreeSet(i, big - 1))
      big = big - 1
      if (i % 1000 == 0) {
        aSortedSet.until(i)
      }
    }
  }
  
  def similarExample {
    var big = 100

    var aSortedSet: SortedSet[Int] = TreeSet(big)

    for (i <- 1 until 10000) {
      aSortedSet = (aSortedSet - big) ++ (TreeSet(i, big - 1)) + big
      big = big - 1
      if (i % 1000 == 0) {
        aSortedSet.until(i)
      }
    }
  }
  
}


