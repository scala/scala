import scala.util.continuations._ 
 
object Test { 

  def foo() = {
    lazy val x = shift((k:Unit=>Unit)=>k())
    println(x)
  }
 
  def main(args: Array[String]) { 
    reset {
      foo()
    }
  } 
	
}