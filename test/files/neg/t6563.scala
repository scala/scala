class A{
  def b(c: => Unit): Unit ={}
  b{
    e("f")
    new G()(){}
 }
}
class G(h:String="i")()
