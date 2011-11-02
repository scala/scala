import _root_.scala.collection.Seq 
import _root_.scala.util.control.Exception 
import _root_.scala.util.continuations._ 
 
object Test { 
 
  trait AbstractResource[+R <: AnyRef]  { 
    def reflect[B] : R @cpsParam[B,Either[Throwable, B]] = shift(acquireFor) 
    def acquireFor[B](f :  R => B) : Either[Throwable, B] = { 
	      import Exception._ 
	      catching(List(classOf[Throwable]) : _*) either (f(null.asInstanceOf[R])) 
	    } 
	  }   
	 
	  def main(args: Array[String]) : Unit = { 
	     val x = new AbstractResource[String] { } 
	     val result = x.acquireFor( x =>  7 ) 
	     println(result) 
	  } 
	} 
