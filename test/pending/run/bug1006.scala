object foo {
  def main(args: Array[String]) {
    abstract class A[T] {
     def myVal: T
    }

    class B[T](value: T) extends A[T] {
      def myVal = value
    }

    Console.println(new B[int](23).myVal)
  }
}
