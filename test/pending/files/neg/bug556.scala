object Main extends Application {
  def f(a:Int=>Int):Int = a(4)
  def g:Int = f((x,y)=>x)
}
