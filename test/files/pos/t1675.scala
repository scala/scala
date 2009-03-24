package a
object Foo extends pack.Bar {
 for(i <- 0 to 10) {
   test("")
 }
}
package pack {
  class Bar {
    protected def test(s: String*) {}
  }
}
