object Texts{
  def textL[T](list: List[T]) = {     
    list match{                        
      case List() => "Empty"              
      case List(_) => "One"      
      case List(_*) => "Many"
    }
  }

  def textA[T](array: Array[T]) = {     
    array match{                        
      case Array() => "Empty"              
      case Array(_) => "One"      
      case Array(_*) => "Many"
    }
  }
}

object Test extends App {

  implicit def array2list[T](array: Array[T]) = {
    println(array.toList.size)
    array.toList
  }

  
  println(Texts textL List()); println(Texts textL List(1)); println(Texts textL List(1, 1));

  println(Texts textL Array()); println(Texts textL Array(1)); println(Texts textL Array(1, 1))
}
