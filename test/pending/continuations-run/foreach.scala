// $Id$

import scala.util.continuations._

import scala.util.continuations.Loops._

object Test {
  
  def main(args: Array[String]): Any = {
    
    
    reset {
      
      val list = List(1,2,3,4,5)
      
      for (x <- list.suspendable) {
        
        shift { k: (Unit => Unit) =>
          println(x)
          if (x < 3)
            k()
          else
            println("enough is enough")
        }
        
      }
      
    }
    
    
  }
  
}