class C[T] {                                     
  def foo = 0                                    
}                                                

object Test {
  implicit def AnyToC[T](a: Any): C[T] = new C[T] 
  // was: "macro not expanded"
  Macro {                                         
    "".foo                                         
     ()                                            
  }
}
