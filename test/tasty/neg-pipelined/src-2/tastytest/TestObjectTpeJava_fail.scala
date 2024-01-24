package tastytest

import lib.ObjectTpeJava
import scala.annotation.nowarn

// keep in sync with ObjectTpeJava.java
object TestObjectTpeJava extends scala.App {

  val newOTJ = new ObjectTpeJava

  // val newOTJInner = new ObjectTpeJava.Inner[Int](23, true)

  // newOTJ.meth1(1) // OK
  // newOTJ.meth2(1) // OK
  // newOTJ.meth3(List[Int](1)) // OK
  // newOTJ.meth4(List[Int](1)) // OK
  // newOTJ.meth5(Array[Object]("abc")) // OK
  // newOTJ.meth6(Array[String]("abc")) // Ok
  newOTJ.meth5(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
  newOTJ.meth6(Array[Int](1)) // error: Array[Int] is not a subtype of Array[T & Object]
  // newOTJ.meth7(1) // OK (creates a reference array)
  // newOTJ.meth8(1) // OK (creates a primitive array and copies it into a reference array at Erasure)
  // val li = Array[Int](1)
  // newOTJ.meth7(li: _*) // OK (will copy the array at Erasure)
  // newOTJ.meth8(li: _*) // OK (will copy the array at Erasure)

  // newOTJInner.meth1(1) // OK
  // newOTJInner.meth2(1) // OK

  // assert((newOTJInner.field1: Int) == 23) // OK
  // newOTJInner.field1 = 31 // OK
  // assert((newOTJInner.getter1: Int) == 31) // OK
  // assert(newOTJInner.field2 == true) // OK
  // newOTJInner.field2 = false // OK
  // assert(newOTJInner.getter2 == false) // OK
}

