class BipClass { }
trait BipTrait {
  self: BipClass =>
  
  private[this] def foo() = 5  
  def bar() = this.foo()
}
// error: value foo is not a member of BipTrait with BipClass
//   def bar() = this.foo()
//                    ^
