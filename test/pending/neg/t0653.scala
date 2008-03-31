class InL[A, B]
class Fix[Op[A]](x : Op[Fix[Op]])

class FixTest {
   val zero = new Fix(new InL)
}
