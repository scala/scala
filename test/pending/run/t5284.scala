object Test {
  def main(args:Array[String]) {
    val a = Blarg(Array(1,2,3))
    println(a.m((x:Int) => x+1))
  }
}

object Blarg {
  def apply[T:Manifest](a:Array[T]) = new Blarg(a)
}
class Blarg [@specialized T:Manifest](val a:Array[T]) {
  def m[@specialized W>:T,@specialized S](f:W=>S) = f(a(0))
}

