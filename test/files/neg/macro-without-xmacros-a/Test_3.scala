object Test extends App {
  import Macros.Shmacros._
  println(foo(2) + Macros.bar(2) * new Macros().quux(4))
}