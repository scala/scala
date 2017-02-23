object Test extends App {
  def baz = {
    def woo(implicit c: scala.reflect.SourceContext) = c
    woo
  }
  def foo = {
    def infix_bar(x:Int,y:Int)(implicit c: scala.reflect.SourceContext) = c
    7 bar 8
  }
  println(baz)
  println(foo)
}
