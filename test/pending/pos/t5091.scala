object RecursiveValueNeedsType {

  def foo(param: String) = 42
  def bar(n: Int) = 42

  {
    val xxx = foo(param = null)
    val param = bar(xxx)
  }
  
}
