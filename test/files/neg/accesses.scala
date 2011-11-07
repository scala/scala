package test.p1.p2

abstract class A {
  private[p2] def f2(): Unit
  protected def f3(): Unit
  private[p1] def f4(): Unit
  protected[p1] def f5(): Unit
}
  
abstract class OK1 extends A {
  private[p1] def f2(): Unit
  protected[p2] def f3(): Unit
  private[test] def f4(): Unit
  protected[test] def f5(): Unit
}
abstract class OK2 extends A {
  protected[p1] def f2(): Unit
  def f3(): Unit
  protected[p1] def f4(): Unit
  def f5(): Unit
}
abstract class Err1 extends A {
  private def f2(): Unit = ()
  private[p2] def f3(): Unit = ()
  private[p2] def f4(): Unit
  protected[p2] def f5(): Unit
}
