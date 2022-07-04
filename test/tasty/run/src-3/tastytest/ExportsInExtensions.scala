package tastytest

object ExportsInExtensions:

  class C(x: Int):
    def bar = x
    def baz(y: Int) = x % y
    val bam = x * x
    def :: (y: Int) = x - y

  extension (x: Int)
    private def cm = new C(x)
    export cm.*
