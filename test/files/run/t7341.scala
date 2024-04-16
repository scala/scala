//> using options -Xcheckinit
//
object Obj {
  private var cache: Any = ()
  def returning(f: () => Unit) = ()
  def foo: Unit = {
    returning(() => cache = ())
  }

  def apply(): Any = {
    cache
  }
}

object Test extends App {
  Obj()
}
