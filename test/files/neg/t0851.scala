package test

// This gives now type errors about missing parameters, which seems OK to me.
// The tests just make sure it does not crash

object test1 {
  case class Foo[T,T2](f : (T,T2) => String) extends (((T,T2)) => String){
    def apply(t : T) = (s:T2) => f(t,s)
    def apply(p : (T,T2)) = f(p._1,p._2)
  }
  implicit def g[T](f : (T,String) => String) = Foo(f)
  def main(args : Array[String]) : Unit = {
    val f = (x:Int,s:String) => s + x
    println(f(1))
    ()
  }
}
object Main {
  def main(args : Array[String]) {
    val fn = (a : Int, str : String) => "a: " + a + ", str: " + str
    implicit def fx[T](f : (T,String) => String) = (x:T) => f(x,null)
    println(fn(1))
    ()
  }
}
