class C(a: Any)
class Test {  
  def foo: Any = {
    def form = 0
    class C1 extends C({def x = form; ()})
  }
}
