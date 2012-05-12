import java.util.Date

trait TDate 

trait TT[A1,T1]

trait TTFactory[F,G] {
  def create(f: F) : TT[F,G]
  def sample: F
}

object Impls {

  // If the c1 is declared before c2, it compiles fine
  implicit def c2(s: Date) = c1.create(s)  

  implicit val c1 = new TTFactory[Date,TDate] {
    def create(v: Date): TT[Date,TDate] = sys.error("")
    def sample = new Date
  }
}