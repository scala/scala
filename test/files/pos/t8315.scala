object Test {
  def crash(as: Listt): Unit = {
    map(as, (_: Any) => return)
  }
 
  final def map(x: Listt, f: Any => Any): Any = {
    if (x eq Nill) "" else f("")
  }
}
 
object Nill extends Listt
class Listt
