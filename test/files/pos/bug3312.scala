trait Root {
  def say: String
}

trait A extends Root {
  override def say: String = "bow"
}

trait B extends Root {
  override def say: String = "hi"
}

object Foo extends A with B  {
  override def say: String = foo(super[A].say)
  
  def foo(p: => String): String = p
}
