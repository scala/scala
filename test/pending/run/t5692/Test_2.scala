object Test extends App {
  val x = Macros.decl
  def y() { Macros.decl(); }
}