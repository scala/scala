
class Test

object Test {
  private implicit class `for your eyes only`(i: Int) {       // warn
    def f = i
  }
}

class Test2 {
  import Test2._
  println(5.toStr)
}
 
object Test2 {
  // was: warning: private object in object Test2 is never used
  // i.e. synthetic object C
  private implicit class C(val i: Int) extends AnyVal {       // no warn
    def toStr = i.toString
  }
}

class Test3 {
  import Test3._
  //println(5.toStr)
}
 
object Test3 {
  // was: warning: private object in object Test2 is never used
  // i.e. synthetic object C
  private implicit class C(val i: Int) extends AnyVal {       // warn
    def toStr = i.toString
  }
}

object Test4 {
  class A { class B }

  private val a: A = new A

  def b = (new a.B).##
}
