object `package` {
  trait Score { def toString : String }
  trait Test[+T <: Score] { def apply(s : String) : T }

  case class FT(f : Float) extends Score
  implicit object FT extends Test[FT] { def apply(s : String) : FT = new FT(s.toFloat) }

  case class IT(i : Int) extends Score
  implicit object IT extends Test[IT] { def apply(s : String) : IT = new IT(s.toInt) }
}

class TT[+T <: Score](implicit val tb : Test[T]) {
  def read(s : String) : T = tb(s)
}

object Tester {
  val tt = new TT[FT]
  val r = tt.read("1.0")
  r.toString
}