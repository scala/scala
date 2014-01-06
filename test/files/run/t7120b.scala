
import scala.language.higherKinds

trait Base[A] { type B = A; }
class C extends Base[String] {
  class D {
    def foo[B1 <: B](b: B1) = 0
  }
}

trait BaseHK[M[_], A] { type B = M[A]; }
object BaseHK { type Id[X] = X }
class CHK extends BaseHK[BaseHK.Id, String] {
  class D {
    def foo[B1 <: B](b: B1) = 0
  }
}


object Test extends App {
  val c = new C
  val d = new c.D()
  val meth = d.getClass.getMethods.find(_.getName == "foo").get
  println(meth)

  val chk = new CHK
  val dhk = new chk.D()
  val methhk = d.getClass.getMethods.find(_.getName == "foo").get
  println(methhk)
}
