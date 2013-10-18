import scala.reflect.macros._

class A[C <: Context with Singleton](position: C#Position)

object A {
  def apply(c: Context)(in: c.Tree): A[c.type] = new A(in.pos)
}
