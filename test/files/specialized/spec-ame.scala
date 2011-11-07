// ticket #3432
object Test {
  trait B[@specialized(Int) T] {
    def value: T 
  }

  class A[@specialized(Int) T](x: T) { 
    def foo: B[T] = new B[T] { 
      def value = x 
    } 
  }

  def main(args: Array[String]) {
    println((new A("abc")).foo.value)
    println((new A(10)).foo.value)
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}
