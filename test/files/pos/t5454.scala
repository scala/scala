object IllegalInheritance {
  trait A
  implicit def a = new A {}            // def => val
  //val r = implicitly[A]              // uncomment
 
  class B[T](t : T)(implicit a : A)    // remove implicit param block
 
  class C extends B/*[Int]*/(23)       // uncomment
  val c = new C                        // comment
}
