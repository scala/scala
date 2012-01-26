// macros should be built separately from their clients, so simple "scalac Printf.scala Test.scala" won't work
// 1) first build the printf macro with "scalac -Xmacros Printf.scala"
// 2) the build this file with "scalac -cp <output directory of compiling Printf.scala> Test.scala"

object Test extends App {
  import Printf._
  printf("hello %s", "world")
}