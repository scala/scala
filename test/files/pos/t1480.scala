class Foo{
  def compare(newP : Any, oldP : Any) : Boolean = (newP,oldP) match {
    case (newP : AnyRef, oldP : AnyRef) if newP == oldP => newP == oldP
    case (newS : Symbol, oldS: Symbol) if newS == oldS => newS == oldS
  }
}
