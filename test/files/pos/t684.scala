package test;
trait Test {
  trait Ti;
  class Foo;
  def foo(t : Ti) = t match {
  case t : Foo => true;
  case _ => false;
  }
  class Bar extends Foo with Ti;
  assert(foo(new Bar));
}
// t684.scala:6:
//   case t : Foo => true;
//            ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.this.Ti
//         pt  Test.this.Ti
//      pattp  Test.this.Foo
//   pattp+pt  Test.this.Foo with Test.this.Ti
//   pt+pattp  Test.this.Ti with Test.this.Foo
//     result  Test.this.Ti with Test.this.Foo
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }