// After lub modification
import scala.collection.mutable.ListBuffer

class A {
  def foo[T](a:T, b:T):T = a
  def f1 = foo(ListBuffer(), List())
  def f2 = foo(ListBuffer(), ListBuffer())
  def f3 = foo(List(), List())
  
  // scalap
  // def f1 : scala.collection.Seq[scala.Nothing] = { /* compiled code */ }
  // def f2 : scala.collection.mutable.ListBuffer[scala.Nothing] = { /* compiled code */ }
  // def f3 : scala.collection.immutable.List[scala.Nothing] = { /* compiled code */ }
}