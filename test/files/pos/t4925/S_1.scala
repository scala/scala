class A {
  final class Inner {
    @inline def foo = 7
  }
  def inner = new Inner 
}
