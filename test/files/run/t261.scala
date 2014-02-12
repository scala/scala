trait A { val foo: String = "A" }
trait B {
   private val foo: String = "B"
   def f = println(foo)
}
object Test extends A with B {
   def main(args: Array[String]) = {
     println(foo)
     f
   }
}