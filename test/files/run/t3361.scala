object Test extends App {
  import scala.collection.mutable.DoubleLinkedList

  empty
  builder_1
  builder_2
  chaining_1
  chaining_2
  insert_1
  insert_2
  append_1
  append_2

  def empty {
    val none = DoubleLinkedList()
    require(none.size == 0)
    none.foreach( _ => require(false))
  }

  def builder_1 {
    val ten = DoubleLinkedList(1 to 10: _*)
    require(10 == ten.size)
  }

  def builder_2 {
    val ten = DoubleLinkedList(1 to 10: _*)
    require((ten.size*(ten.size+1))/2 == ten.reduceLeft(_ + _))
  }

  def chaining_1 {
    val ten = DoubleLinkedList(1 to 10: _*)
    require(ten.reverse == DoubleLinkedList((1 to 10).reverse: _*))
  }

  def chaining_2 {
    val ten = DoubleLinkedList(1 to 10: _*)
    require(ten == ten.reverse.reverse)
  }

  def insert_1 {
    val ten = DoubleLinkedList(1 to 10: _*)
    ten.append(DoubleLinkedList(11))
    
    // Post-insert size test
    require(11 == ten.size)
    // Post-insert data test
    require((ten.size*(ten.size+1))/2 == ten.reduceLeft(_ + _))
    // Post-insert chaining test
    require(ten == ten.reverse.reverse)
    // Post-insert position test
    require(ten.last == 11)
  }

  def insert_2 {
    val ten = DoubleLinkedList(1 to 10: _*)
    try {
      DoubleLinkedList().insert(ten)
    } catch {
      case _: IllegalArgumentException => require(true)
      case _ => require(false)
    }
    val zero = DoubleLinkedList(0)
    zero.insert(ten)
    require(zero.size == 11)
    require(zero.head == 0)
    require(zero.last == 10)
  }

  def append_1 {
    val ten = DoubleLinkedList(1 to 10: _*)
    val eleven = ten.append(DoubleLinkedList(11))
    // Post-append equality test
    require(ten == eleven)
    // Post-append size test
    require(11 == ten.size)
    // Post-append data test
    require((ten.size*(ten.size+1))/2 == ten.reduceLeft(_ + _))
    // Post-append chaining test
    require(ten == ten.reverse.reverse)
    // Post-append position test
    require(ten.last == 11)
  }

  def append_2 {
    val ten = DoubleLinkedList(1 to 10: _*)
    try {
      DoubleLinkedList().append(ten)
    } catch {
      case _: IllegalArgumentException => require(true)
      case _ => require(false)
    }
    val zero = DoubleLinkedList(0)
    zero.append(ten)
    require(zero.size == 11)
    require(zero.head == 0)
    require(zero.last == 10)
  }
}
