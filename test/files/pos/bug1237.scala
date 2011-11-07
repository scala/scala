class HelloWorld {  
  def main(args: Array[String]) { 

    object TypeBool;

    trait Fct {
      def g(x : Int) = TypeBool // breaks.
      
      //    def g(x : Int) = 3 // fine.
    }

    ()
  }
}
