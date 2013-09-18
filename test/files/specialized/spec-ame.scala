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
    // before fixing SI-7343, this was printing 3. Now it's printing 2,
    // since the anonymous class created by doing new B[T] { ... } when
    // T = Int is now rewired to B$mcI$sp instead of just B[Int]
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}
