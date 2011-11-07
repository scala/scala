package test
class T {    
  def foo[T <: String](x: T): T = x
  def bar[T](x: T): T = x
  class Inner {
    def foo[T](x: T): T = x
    def bar[T](x: T): T = x
  }
}

