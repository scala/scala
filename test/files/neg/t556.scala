object Main extends App {
  def f(a:Int=>Int):Int = a(4)
  def g:Int = f((x,y)=>x)
}
