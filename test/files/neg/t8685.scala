

@deprecated("class C is depr", since="now")
case class C(i: Int)

case class D @deprecated("ctor D is depr", since="now") (i: Int)

@deprecated("class E is depr", since="now")
case class E(i: Int)
@deprecated("module E is depr", since="now")
object E

@deprecated("module F is depr", since="now")
object F {
  case class G(i: Int)
}

object G {
  case class H(i: Int)
}

object Extra {
  @deprecated("Extra module F is depr", since="now")
  object F {
    case class G(i: Int)
  }
}

object J {
  @deprecated("Inner K is depr", since="now")
  case class K(i: Int)
}

trait Applies {
  def f = C(42)
  def g = D(42)
  def h = E(42)
  def i = F.G(42)
  def j = Extra.F.G(42)

  @deprecated("member gg", since="now")
  val gg = G
  def k = this.gg.H(0)

  def l = J.K(42)
}
trait News {
  def f = new C(42)
  def g = new D(42)
  def h = new E(42)
  def i = new F.G(42)
  def j = new Extra.F.G(42)
  def l = new J.K(42)
}
