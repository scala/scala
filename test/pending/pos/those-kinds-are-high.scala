class A {
  trait Container[+T]
  trait Template[+CC[X] <: Container[X]]

  class C1[T] extends Template[C1] with Container[T]
  class C2[T] extends Template[C2] with Container[T]
  
  /** Target expression:
   *    List(new C1[String], new C2[String])
   */
    
  // Here's what would ideally be inferred.
  //
  // scala> :type List[Template[Container] with Container[String]](new C1[String], new C2[String])
  // List[Template[Container] with Container[java.lang.String]]
  //
  // Here's what it does infer.
  // 
  // scala> :type List(new C1[String], new C2[String])
  // <console>:8: error: type mismatch;
  //  found   : C1[String]
  //  required: Container[String] with Template[Container[Any] with Template[Container[Any] with Template[Any] with ScalaObject] with ScalaObject] with ScalaObject
  //        List(new C1[String], new C2[String])
  //             ^
  //
  // Simplified, the inferred type is:
  //
  //   List[Container[String] with Template[Container[Any] with Template[Container[Any] with Template[Any]]]
  //
  
  /** Working version explicitly typed.
   */
  def fExplicit = List[Template[Container] with Container[String]](new C1[String], new C2[String])
  
  // nope
  // def fFail = List(new C1[String], new C2[String])
}
