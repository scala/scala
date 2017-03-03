trait SamRepeated { def accept(a: Any*): Unit }
trait SamByName { def accept(a: => Any): (Any, Any) }

object Test extends App {
  val rep: SamRepeated = (a) => println(a)
  rep.accept(1)

  val nam: SamByName = (a) => (a, a)
  var v = 0
  assert(nam.accept({v += 1; v}) == (1, 2))
  assert(v == 2, "by name arg should be evaluated twice")
}
