class HelloWorld {
  def main(args: Array[String]) {

    object TypeBool;

    trait Fct
    {
      def g(x : int) = TypeBool // breaks.

      //    def g(x : int) = 3 // fine.
    }

    ()
  }
}
