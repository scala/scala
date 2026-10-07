//> using options -Werror -Wunused:privates
object Test {
  private case class Unused(i: Int = 42)

  private case class C()
  private object C {
    val c = new C().toString // obviously a usage; rhs is not in ctor at typer
  }

  private case class D()
  private object D {
    new D() // D for ditto
  }
}
