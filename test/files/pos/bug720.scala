trait Conv
object Conv {
  implicit def one2two (one: One): Two = new Two }
class One extends Conv
class Two
object Test2 extends App {
  def fun (two: Two) = ()
  fun(new One)
}
