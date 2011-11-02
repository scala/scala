


import scala.collection.mutable.PriorityQueue






// populate a priority queue a few different ways and make sure they all seem equal
object Test {

  def main(args: Array[String]) {
    // testInsertionsAndEqualities
    // testIntensiveEnqueueDequeue
    // testTails
    // testInits
    // testFilters
    // testDrops
    // testEquality
    // testMisc
    // testReverse
    // testToList
    // testForeach
  }

  // def testInsertionsAndEqualities {
  //   import scala.util.Random.nextInt
  //   val pq1 = new PriorityQueue[String]
  //   val pq2 = new PriorityQueue[String]
  //   val pq3 = new PriorityQueue[String]
  //   val pq4 = new PriorityQueue[String]
    
  //   val strings = (1 to 20).toList map (i => List.fill((Math.abs(nextInt % 20)) + 1)("x").mkString)
    
  //   pq1 ++= strings
  //   pq2 ++= strings.reverse
  //   for (s <- strings) pq3 += s
  //   for (s <- strings.reverse) pq4 += s
    
  //   val pqs = List(pq1, pq2, pq3, pq4, pq1.clone, pq2.clone)
  
  //   for (queue1 <- pqs ; queue2 <- pqs) {
  //     val l1: List[String] = queue1.dequeueAll[String, List[String]]
  //     val l2: List[String] = queue2.dequeueAll[String, List[String]]
  //     assert(l1 == l2)
  //     assert(queue1.max == queue2.max)
  //   }
    
  //   assertPriorityDestructive(pq1)
  // }

  // not a sequence anymore, Mildred
  // def testIndexing {
  //   val pq = new PriorityQueue[Char]
  //   "The quick brown fox jumps over the lazy dog".foreach(pq += _)

  //   // val iter = pq.iterator
  //   // while (iter.hasNext) println("`" + iter.next + "`")
  //   assert(pq(0) == 'z')
  //   assert(pq(1) == 'y')
  //   assert(pq(2) == 'x')
  //   assert(pq(3) == 'w')
  //   assert(pq(4) == 'v')
  //   assert(pq(5) == 'u')
  //   assert(pq(7) == 't')
  //   assert(pq(8) == 's')
  //   assert(pq(9) == 'r')
  //   assert(pq(10) == 'r')

  //   pq.clear
  //   "abcdefghijklmnopqrstuvwxyz".foreach(pq += _)
  //   for (i <- 0 until 26) assert(pq(i) == ('z' - i))

  //   val intpq = new PriorityQueue[Int]
  //   val intlst = new collection.mutable.ArrayBuffer ++ (0 until 100)
  //   val random = new util.Random(101)
  //   while (intlst.nonEmpty) {
  //     val idx = random.nextInt(intlst.size)
  //     intpq += intlst(idx)
  //     intlst.remove(idx)
  //   }
  //   for (i <- 0 until 100) assert(intpq(i) == (99 - i))
  // }
  
  // def testTails {
  //   val pq = new PriorityQueue[Int]
  //   for (i <- 0 until 10) pq += i * 4321 % 200

  //   assert(pq.size == 10)
  //   assert(pq.nonEmpty)

  //   val tailpq = pq.tail
  //   // pq.printstate
  //   // tailpq.printstate
  //   assert(tailpq.size == 9)
  //   assert(tailpq.nonEmpty)
  //   assertPriorityDestructive(tailpq)
  // }

  // def assertPriorityDestructive[A](pq: PriorityQueue[A])(implicit ord: Ordering[A]) {
  //   import ord._
  //   var prev: A = null.asInstanceOf[A]
  //   while (pq.nonEmpty) {
  //     val curr = pq.dequeue
  //     if (prev != null) assert(curr <= prev)
  //     prev = curr
  //   }
  // }
  
  // def testInits {
  //   val pq = new PriorityQueue[Long]
  //   for (i <- 0 until 20) pq += (i + 313) * 111 % 300
    
  //   assert(pq.size == 20)
    
  //   val initpq = pq.init
  //   assert(initpq.size == 19)
  //   assertPriorityDestructive(initpq)
  // }

  // def testFilters {
  //   val pq = new PriorityQueue[String]
  //   for (i <- 0 until 100) pq += "Some " + (i * 312 % 200)
    
  //   val filpq = pq.filter(_.indexOf('0') != -1)
  //   assertPriorityDestructive(filpq)
  // }

  // def testIntensiveEnqueueDequeue {
  //   val pq = new PriorityQueue[Int]
    
  //   testIntensive(1000, pq)
  //   pq.clear
  //   testIntensive(200, pq)
  // }
  
  // def testIntensive(sz: Int, pq: PriorityQueue[Int]) {
  //   val lst = new collection.mutable.ArrayBuffer[Int] ++ (0 until sz)
  //   val rand = new util.Random(7)
  //   while (lst.nonEmpty) {
  //     val idx = rand.nextInt(lst.size)
  //     pq.enqueue(lst(idx))
  //     lst.remove(idx)
  //     if (rand.nextDouble < 0.25 && pq.nonEmpty) pq.dequeue
  //     assertPriority(pq)
  //   }
  // }

  // def testDrops {
  //   val pq = new PriorityQueue[Int]
  //   pq ++= (0 until 100)
  //   val droppq = pq.drop(50)
  //   assertPriority(droppq)
    
  //   pq.clear
  //   pq ++= droppq
  //   assertPriorityDestructive(droppq)
  //   assertPriority(pq)
  //   assertPriorityDestructive(pq)
  // }

  // // your sequence days have ended, foul priority queue
  // // def testUpdates {
  // //   val pq = new PriorityQueue[Int]
  // //   pq ++= (0 until 36)
  // //   assertPriority(pq)

  // //   pq(0) = 100
  // //   assert(pq(0) == 100)
  // //   assert(pq.dequeue == 100)
  // //   assertPriority(pq)

  // //   pq.clear
    
  // //   pq ++= (1 to 100)
  // //   pq(5) = 200
  // //   assert(pq(0) == 200)
  // //   assert(pq(1) == 100)
  // //   assert(pq(2) == 99)
  // //   assert(pq(3) == 98)
  // //   assert(pq(4) == 97)
  // //   assert(pq(5) == 96)
  // //   assert(pq(6) == 94)
  // //   assert(pq(7) == 93)
  // //   assert(pq(98) == 2)
  // //   assert(pq(99) == 1)
  // //   assertPriority(pq)

  // //   pq(99) = 450
  // //   assert(pq(0) == 450)
  // //   assert(pq(1) == 200)
  // //   assert(pq(99) == 2)
  // //   assertPriority(pq)

  // //   pq(1) = 0
  // //   assert(pq(1) == 100)
  // //   assert(pq(99) == 0)
  // //   assertPriority(pq)
  // //   assertPriorityDestructive(pq)
  // // }

  // def testEquality {
  //   val pq1 = new PriorityQueue[Int]
  //   val pq2 = new PriorityQueue[Int]
    
  //   pq1 ++= (0 until 50)
  //   var i = 49
  //   while (i >= 0) {
  //     pq2 += i
  //     i -= 1
  //   }
  //   assert(pq1 == pq2)
  //   assertPriority(pq2)
    
  //   pq1 += 100
  //   assert(pq1 != pq2)
  //   pq2 += 100
  //   assert(pq1 == pq2)
  //   pq2 += 200
  //   assert(pq1 != pq2)
  //   pq1 += 200
  //   assert(pq1 == pq2)
  //   assertPriorityDestructive(pq1)
  //   assertPriorityDestructive(pq2)
  // }

  // def testMisc {
  //   val pq = new PriorityQueue[Int]
  //   pq ++= (0 until 100)
  //   assert(pq.size == 100)
    
  //   val (p1, p2) = pq.partition(_ < 50)
  //   assertPriorityDestructive(p1)
  //   assertPriorityDestructive(p2)

  //   val spq = pq.slice(25, 75)
  //   assertPriorityDestructive(spq)

  //   pq.clear
  //   pq ++= (0 until 10)
  //   pq += 5
  //   assert(pq.size == 11)

  //   val ind = pq.lastIndexWhere(_ == 5)
  //   assert(ind == 5)
  //   assertPriorityDestructive(pq)

  //   pq.clear
  //   pq ++= (0 until 10)
  //   assert(pq.lastIndexWhere(_ == 9) == 0)
  //   assert(pq.lastIndexOf(8) == 1)
  //   assert(pq.lastIndexOf(7) == 2)
    
  //   pq += 5
  //   pq += 9
  //   assert(pq.lastIndexOf(9) == 1)
  //   assert(pq.lastIndexWhere(_ % 2 == 1) == 10)
  //   assert(pq.lastIndexOf(5) == 6)
    
  //   val lst = pq.reverseIterator.toList
  //   for (i <- 0 until 5) assert(lst(i) == i)
  //   assert(lst(5) == 5)
  //   assert(lst(6) == 5)
  //   assert(lst(7) == 6)
  //   assert(lst(8) == 7)
  //   assert(lst(9) == 8)
  //   assert(lst(10) == 9)
  //   assert(lst(11) == 9)
    
  //   pq.clear
  //   assert(pq.reverseIterator.toList.isEmpty)
    
  //   pq ++= (50 to 75)
  //   assert(pq.lastIndexOf(70) == 5)
    
  //   pq += 55
  //   pq += 70
  //   assert(pq.lastIndexOf(70) == 6)
  //   assert(pq.lastIndexOf(55) == 22)
  //   assert(pq.lastIndexOf(55, 21) == 21)
  //   assert(pq.lastIndexWhere(_ > 54) == 22)
  //   assert(pq.lastIndexWhere(_ > 54, 21) == 21)
  //   assert(pq.lastIndexWhere(_ > 69, 5) == 5)
  // }
  
  // def testReverse {
  //   val pq = new PriorityQueue[(Int, Int)]
  //   pq ++= (for (i <- 0 until 10) yield (i, i * i % 10))
    
  //   assert(pq.reverse.size == pq.reverseIterator.toList.size)
  //   assert((pq.reverse zip pq.reverseIterator.toList).forall(p => p._1 == p._2))
  //   assert(pq.reverse.sameElements(pq.reverseIterator.toSeq))
  //   assert(pq.reverse(0)._1 == pq(9)._1)
  //   assert(pq.reverse(1)._1 == pq(8)._1)
  //   assert(pq.reverse(4)._1 == pq(5)._1)
  //   assert(pq.reverse(9)._1 == pq(0)._1)
    
  //   pq += ((7, 7))
  //   pq += ((7, 9))
  //   pq += ((7, 8))
  //   assert(pq.reverse.reverse == pq)
  //   assert(pq.reverse.lastIndexWhere(_._2 == 6) == 6)
  //   assertPriorityDestructive(pq.reverse.reverse)
    
  //   val iq = new PriorityQueue[Int]
  //   iq ++= (0 until 50)
  //   assert(iq.reverse == iq.reverseIterator.toSeq)
  //   assert(iq.reverse.reverse == iq)
    
  //   iq += 25
  //   iq += 40
  //   iq += 10
  //   assert(iq.reverse == iq.reverseIterator.toList)
  //   assert(iq.reverse.reverse == iq)
  //   assert(iq.reverse.lastIndexWhere(_ == 10) == 11)
  //   assertPriorityDestructive(iq.reverse.reverse)
  // }
  
  // def testToList {
  //   val pq = new PriorityQueue[Int]
    
  //   pq += 1
  //   pq += 4
  //   pq += 0
  //   pq += 5
  //   pq += 3
  //   pq += 2
  //   assert(pq.toList == pq)
  //   assert(pq == List(5, 4, 3, 2, 1, 0))
  //   assert(pq.reverse == List(0, 1, 2, 3, 4, 5))
    
  //   pq.clear
  //   for (i <- -50 until 50) pq += i
  //   assert(pq.toList == pq)
  //   assert(pq.toList == (-50 until 50).reverse)
  // }
  
  // def testForeach {
  //   val pq = new PriorityQueue[Char]
    
  //   pq += 't'
  //   pq += 'o'
  //   pq += 'b'
  //   pq += 'y'
  //   val sbf = new StringBuilder
  //   val sbi = new StringBuilder
  //   pq.foreach(sbf += _)
  //   pq.iterator.foreach(sbi += _)
  //   assert(sbf.toString == sbi.toString)
  //   assert(sbf.toString == "ytob")
  // }
  
}


















