class RefinementAndExistentials {
  type Foo = {
    type Dingus
    def bippy(x: Int): String
    def dingus(): String
  }
  type Bar = {
    type Dingus <: T forSome { type T <: String }
  }
  def f(x: Foo) = 51
  def g(x: T forSome { type T <: String }) = x
  def h(x: Float): { def quux(x: Int, y: Int): Int } = new {
    def quux(x: Int, y: Int) = 55
  }
  def hh(x: Float) = new { def quux(x: Int, y: Int) = 55 }
  def j(x: Int): Bar = sys.error("")
  def k(): AnyRef { type Dingus <: T forSome { type T <: String } } = sys.error("")
}
