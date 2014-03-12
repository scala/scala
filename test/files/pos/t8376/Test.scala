class Test {
  BindingsX.select("", "") // okay in 2.10, fails in 2.11

  BindingsY.select1("", "") // okay in both
}

object BindingsY {
  def select1(root: String, steps: String*) = ()  
  def select1(root: Any, steps: String*) = ()
}
