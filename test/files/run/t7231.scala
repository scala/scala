object Test extends App {
   val bar: Null = null

   def foo(x: Array[Int]) = x
   def baz(x: String) = x

   // first line was failing
   println(foo(bar))
   // this line worked but good to have a double check
   println(baz(bar))
}