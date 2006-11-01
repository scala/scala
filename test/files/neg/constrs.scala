object test {

  abstract class Test(x: int) {
    type T;
    val u = x;
    def this(y: int)(z: int)(t: this.T) = { this(this.u + y + z); Console.println(x) }
  }

  class Foo(x: int) {
    def this() = this("abc")
    def this(x: String) = this(1)
    def this(x: boolean) = this(x)
  }

  class Bar[a](x: a) {
    def this() = this(1)
  }

}
