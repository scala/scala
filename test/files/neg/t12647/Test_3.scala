
//> using options -Xsource:3

object Test extends App {
  val resolver = new ValueResolver
  println(resolver.resolve.value)
}
