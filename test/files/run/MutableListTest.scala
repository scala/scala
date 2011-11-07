


import scala.collection.mutable.MutableList



class ExtList[T] extends MutableList[T] {
  def printState {
    println("Length: " + len)
    println("Last elem: " + last0.elem)
    println("First elem: " + first0.elem)
    println("After first elem: " + first0.next.elem)
    println("After first: " + first0.next)
    println("Last: " + last0)
  }
}

object Test {

  def main(args: Array[String]) {
    testEmpty
    testAddElement
    testAddFewElements
    testAddMoreElements
    testTraversables
  }

  def testEmpty {
    val mlist = new MutableList[Int]
    assert(mlist.isEmpty)
    assert(mlist.get(0) == None)
  }

  def testAddElement {
    val mlist = new MutableList[Int]
    mlist += 17
    assert(mlist.nonEmpty)
    assert(mlist.length == 1)
    assert(mlist.head == 17)
    assert(mlist.last == 17)
    assert(mlist(0) == 17)
    assert(mlist.tail.isEmpty)
    assert(mlist.tail.length == 0)
    mlist(0) = 101
    assert(mlist(0) == 101)
    assert(mlist.toList == List(101))
    assert(mlist.tail.get(0) == None)
    assert((mlist.tail += 19).head == 19)
    assert(mlist.tail.length == 0)
  }

  def testAddFewElements {
    val mlist = new MutableList[Int]
    for (i <- 0 until 2) mlist += i
//    mlist.printState
    for (i <- 0 until 2) assert(mlist(i) == i)
    assert(mlist.length == 2)
    assert(mlist.nonEmpty)
    assert(mlist.tail.length == 1)
    assert(mlist.tail.tail.length == 0)
    assert(mlist.tail.tail.isEmpty)
  }

  def testAddMoreElements {
    val mlist = new MutableList[Int]
    for (i <- 0 until 10) mlist += i * i
    assert(mlist.nonEmpty)
    assert(mlist.length == 10)
    for (i <- 0 until 10) assert(mlist(i) == i * i)
    assert(mlist(5) == 5 * 5)
    assert(mlist(9) == 9 * 9)
    var sometail = mlist
    for (i <- 0 until 10) {
      assert(sometail.head == i * i)
      sometail = sometail.tail
    }
    mlist(5) = -25
    assert(mlist(5) == -25)
    mlist(0) = -1
    assert(mlist(0) == -1)
    mlist(9) = -81
    assert(mlist(9) == -81)
    assert(mlist(5) == -25)
    assert(mlist(0) == -1)
    assert(mlist.last == -81)
    mlist.clear
    assert(mlist.isEmpty)
    mlist += 1001
    assert(mlist.head == 1001)
    mlist += 9999
    assert(mlist.tail.head == 9999)
    assert(mlist.last == 9999)
  }

  def testTraversables {
    val mlist = new MutableList[Int]
    for (i <- 0 until 10) mlist += i * i
    var lst = mlist.drop(5)
    for (i <- 0 until 5) assert(lst(i) == (i + 5) * (i + 5))
    lst = lst.take(3)
    for (i <- 0 until 3) assert(lst(i) == (i + 5) * (i + 5))
    lst += 129
    assert(lst(3) == 129)
    assert(lst.last == 129)
    assert(lst.length == 4)
    lst += 7
    assert(lst.init.last == 129)
    assert(lst.length == 5)
    lst.clear
    assert(lst.length == 0)
    for (i <- 0 until 5) lst += i
    assert(lst.reduceLeft(_ + _) == 10)
  }

}










