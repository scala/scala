/*
  NOTE: if inference is changed so that
        T is inferred to be Int, rather than Nothing,
        the piece of code below will start to compile OK.
        In that case, see ticket #2139, and make sure that
        the generated code will no longer crash!
*/
class U {
  def f[T](x:T):T=x
}
object H extends App {
  val u=new U
  val z:Int=(u.f _)(4)
  println("done")
}
