class A {
  @Test(exc = classOf[Exception])
  def myTestMethod = 0
}
// rytz@chara:~/scala/trunk/sandbox$ javac Test.java 
// rytz@chara:~/scala/trunk/sandbox$ ../build/pack/bin/scalac A.scala 
// A.scala:2: error: type mismatch;
//  found   : java.lang.Class[Exception](classOf[java.lang.Exception])
//  required: java.lang.Class
//   @Test(exc = classOf[Exception])
//                      ^
// one error found