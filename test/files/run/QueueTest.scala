

import scala.collection.mutable.Queue




class ExtQueue[T] extends Queue[T] {
  def printState {
    println("-------------------")
    println("Length: " + len)
    println("First: " + first0)
    println("First elem: " + first0.elem)
    println("After first: " + first0.next)
  }
}

object Test {
  
  def main(args: Array[String]) {
    testEmpty
    testEnqueue
    testTwoEnqueues
    testFewEnqueues
    testMoreEnqueues
  }

  def testEmpty {
    val queue = new Queue[Int]

    assert(queue.isEmpty)
    assert(queue.size == 0)
    assert(queue.length == 0)
    assert(queue.dequeueFirst(_ > 500) == None)
    assert(queue.dequeueAll(_ > 500).isEmpty)

    queue.clear
    assert(queue.isEmpty)
    assert(queue.size == 0)
    assert(queue.length == 0)
    assert(queue.dequeueFirst(_ > 500) == None)
    assert(queue.dequeueAll(_ > 500).isEmpty)    
  }

  def testEnqueue {
    val queue = new Queue[Int]
    
    queue.enqueue(10)
    assert(queue.nonEmpty)
    assert(queue.size == 1)
    assert(queue.length == 1)
    assert(queue.head == 10)
    assert(queue(0) == 10)
    assert(queue.init.isEmpty)
    assert(queue.tail.isEmpty)

    queue.clear
    assert(queue.isEmpty)
    assert(queue.length == 0)

    queue.enqueue(11)
    assert(queue.nonEmpty)
    assert(queue.length == 1)
    assert(queue.head == 11)
    assert(queue.front == 11)
    
    val deq = queue.dequeue
    assert(deq == 11)
    assert(queue.isEmpty)
    assert(queue.length == 0)

    queue.enqueue(12)
    val pdopt = queue.dequeueFirst(_ > 999)
    assert(pdopt == None)
    assert(queue.nonEmpty && queue.length == 1)

    val somepd = queue.dequeueFirst(_ >= 1)
    assert(somepd == Some(12))
    assert(queue.isEmpty && queue.length == 0)
  }

  def testTwoEnqueues {
    val queue = new ExtQueue[Int]
    queue.enqueue(30)
    queue.enqueue(40)
    
    assert(queue.length == 2)
    assert(queue.size == 2)
    assert(queue.nonEmpty)
    assert(queue.front == 30)
//    queue.printState
    
    val all = queue.dequeueAll(_ > 20)
    assert(all.size == 2)
    assert(all.contains(30))
    assert(all.contains(40))
    assert(queue.size == 0)
    assert(queue.isEmpty)
  }

  def testFewEnqueues {
    val queue = new ExtQueue[Int]
    queue.enqueue(10)
    queue.enqueue(20)
    
    assert(queue.length == 2)
    assert(queue.nonEmpty)
    assert(queue.head == 10)
    assert(queue.last == 20)
    assert(queue.front == 10)
//    queue.printState
    
    val ten = queue.dequeue
    assert(ten == 10)
    assert(queue.length == 1)
//    queue.printState
    
    queue.enqueue(30)
//    queue.printState
    val gt25 = queue.dequeueFirst(_ > 25)
    assert(gt25 == Some(30))
    assert(queue.nonEmpty)
    assert(queue.length == 1)
    assert(queue.head == 20)
    assert(queue.front == 20)
//    queue.printState

    queue.enqueue(30)
//    queue.printState
    val lt25 = queue.dequeueFirst(_ < 25)
    assert(lt25 == Some(20))
    assert(queue.nonEmpty)
    assert(queue.length == 1)
//    queue.printState

    queue.enqueue(40)
//    queue.printState
    val all = queue.dequeueAll(_ > 20)
//    queue.printState
    assert(all.size == 2)
    assert(all.contains(30))
    assert(all.contains(40))
    assert(queue.isEmpty)
    assert(queue.length == 0)
    
    queue.enqueue(50)
    queue.enqueue(60)
//    queue.printState
    val allgt55 = queue.dequeueAll(_ > 55)
//    println(allgt55)
//    queue.printState
    assert(allgt55.size == 1)
    assert(allgt55.contains(60))
    assert(queue.length == 1)

    queue.enqueue(70)
    queue.enqueue(80)
//    queue.printState
    val alllt75 = queue.dequeueAll(_ < 75)
//    queue.printState
    assert(alllt75.size == 2)
    assert(alllt75.contains(70))
    assert(alllt75.contains(50))
    assert(queue.length == 1)
    assert(queue.head == 80)
    assert(queue.last == 80)
    assert(queue.front == 80)
  }

  def testMoreEnqueues {
    val queue = new ExtQueue[Int]
    for (i <- 0 until 10) queue.enqueue(i * 2)

    for (i <- 0 until 10) {
      val top = queue.dequeue
      assert(top == i * 2)
      assert(queue.length == 10 - i - 1)
    }
    assert(queue.isEmpty)
    assert(queue.length == 0)
    
    for (i <- 0 until 10) queue.enqueue(i * i)
    assert(queue.length == 10)
    assert(queue.nonEmpty)

    //queue.printState
    val gt5 = queue.dequeueAll(_ > 4)
    //queue.printState
    //println(gt5)
    assert(gt5.size == 7)
    assert(queue.length == 3)
    assert(queue.nonEmpty)

    queue.clear
    assert(queue.length == 0)
    assert(queue.isEmpty)
    
    for (i <- 0 until 10) queue.enqueue(i)
    assert(queue.length == 10)
    
    val even = queue.dequeueAll(_ % 2 == 0)
    assert(even.size == 5)
    assert(even.sameElements(List(0, 2, 4, 6, 8)))
    assert(queue.length == 5)
    assert(queue.head == 1)
    assert(queue.last == 9)

    val odd = queue.dequeueAll(_ %2 == 1)
    assert(odd.size == 5)
    assert(queue.length == 0)
    assert(queue.isEmpty)
    assert(odd.sameElements(List(1, 3, 5, 7, 9)))
    
    for (i <- 0 until 10) queue.enqueue(i * i)
    assert(queue.last == 81)
    assert(queue.head == 0)
    assert(queue.length == 10)
    
    val foddgt25 = queue.dequeueFirst(num => num > 25 && num % 2 == 1)
    assert(foddgt25 == Some(49))
    assert(queue.length == 9)
    assert(queue.nonEmpty)

    //queue.printState
    val lt30 = queue.dequeueAll(_ < 30)
    //println(lt30)
    //queue.printState
    assert(lt30.size == 6)
    assert(queue.length == 3)
    
    val fgt60 = queue.dequeueFirst(_ > 60)
    assert(fgt60 == Some(64))
    assert(queue.length == 2)
    assert(queue.head == 36)
    assert(queue.last == 81)

    val sgt60 = queue.dequeueFirst(_ > 60)
    assert(sgt60 == Some(81))
    assert(queue.length == 1)
    assert(queue.head == 36)
    assert(queue.last == 36)

    val nogt60 = queue.dequeueFirst(_ > 60)
    assert(nogt60 == None)
    assert(queue.length == 1)
    assert(queue.nonEmpty)
    assert(queue.head == 36)

    val gt0 = queue.dequeueFirst(_ > 0)
    assert(gt0 == Some(36))
    assert(queue.length == 0)
    assert(queue.isEmpty)

    for (i <- 0 until 4) queue.enqueue(i)
    val interv = queue.dequeueAll(n => n > 0 && n < 3)
    assert(interv.sameElements(List(1, 2)))
    assert(queue.length == 2)
    assert(queue.head == 0)
    assert(queue.last == 3)

    queue.dequeue
    assert(queue.head == 3)
    
    queue.enqueue(9)
    val three = queue.dequeueFirst(_ < 5)
    assert(three == Some(3))
    assert(queue.length == 1)
    assert(queue.head == 9)

    queue.clear
    for (i <- -100 until 100) queue.enqueue(i * i + i % 7 + 5)
    assert(queue.length == 200)

    val manyodds = queue.dequeueAll(_ % 2 == 1)
    assert((manyodds.size + queue.length) == 200)

    queue.dequeueAll(_ > -10000)
    assert(queue.isEmpty)

    for (i <- 0 until 100) queue.enqueue(i)
    val multof3 = queue.dequeueAll(_ % 3 == 0)
    assert(multof3.size == 34)
    assert(queue.size == 66)
    
    val n98 = queue.dequeueFirst(_ == 98)
    assert(n98 == Some(98))
    assert(queue.size == 65)
    assert(queue.last == 97)
    assert(queue.head == 1)
    // well... seems to work
  }

}




