package test.p1.p2

abstract class A {
  private[p2] def f2: unit
  protected def f3: unit
  private[p1] def f4: unit
  protected[p1] def f5: unit
}

abstract class OK1 extends A {
  private[p1] def f2: unit
  protected[p2] def f3: unit
  private[test] def f4: unit
  protected[test] def f5: unit
}
abstract class OK2 extends A {
  protected[p1] def f2: unit
  def f3: unit
  protected[p1] def f4: unit
  def f5: unit
}
abstract class Err1 extends A {
  private def f2: unit = ()
  private[p2] def f3: unit = ()
  private[p2] def f4: unit
  protected[p2] def f5: unit
}
