object test {

  abstract class Test(x: Int) {
    type T;
    val u = x;
    def this(y: Int)(z: Int)(t: this.T) = { this(this.u + y + z); Console.println(x) }
  }

  class Foo(x: Int) {
    def this() = this("abc")
    def this(x: String) = this(1)
    def this(x: Boolean) = this(x)
  }

  class Bar[a](x: a) {
    def this() = this(1)
  }

}
