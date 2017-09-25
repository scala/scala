trait A
trait B
trait C
trait D

object Test {
  type AB = A with B
  val list: List[AB with C] = Nil
  list.collect { case d: D => d }
}
