// TODO come up with a non-trivial universe different from ru
// an rewrite this test, so that it makes sure that cross-universe implicit searches work
//
// import scala.reflect.{basis => rb}
// import scala.reflect.runtime.{universe => ru}
// object Test {
//   def main(args: Array[String]) {
//     def foo(implicit t: rb.TypeTag[List[Int]]) {
//       println(t)
//       val t2: ru.TypeTag[_] = t in ru.rootMirror
//       println(t2)
//     }
//   }
// }

object Test extends App