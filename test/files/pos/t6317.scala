abstract class C { 
  def overloaded(foo: String, bar: String): String
  def overloaded(foo: String, bar: String, baz: String): Unit
}

class ScalaCompilerKiller {
  implicit def CWrapper(c: C) = new {
    def overloaded(request: Any): Unit = {}
  }

  val sps = List[(String, String)]()

  // to repro, need: implicit conversion, overloading, pair in synthetic scrutinee in function passed to higher-order method
  (null: C).overloaded(sps.map(/* _ match */ { case (r, _) => r }))

  // workaround ...
  (null: C).overloaded(sps.map(_ match { case (r, _) => r }))
}
