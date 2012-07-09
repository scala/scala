
class A {
  def foo(x: String, y: Int) = 1
  def foo(x: Int, y: String) = 2
}

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  val a = new A
  val im = cm.reflect(a)
  val tpe = u.typeOf[A]
  val overloaded = tpe member u.newTermName("foo") asTermSymbol
  val ms1 =
    overloaded resolveOverloaded(nameVargs = Seq((u.newTermName("x"), u.typeOf[String]), (u.newTermName("y"), u.typeOf[Int])))
  val ms2 =
    overloaded resolveOverloaded(nameVargs = Seq((u.newTermName("y"), u.typeOf[Int]), (u.newTermName("x"), u.typeOf[String])))
  val ms3 =
    overloaded resolveOverloaded(nameVargs = Seq((u.newTermName("x"), u.typeOf[Int]), (u.newTermName("y"), u.typeOf[String])))
  val ms4 =
    overloaded resolveOverloaded(nameVargs = Seq((u.newTermName("y"), u.typeOf[String]), (u.newTermName("x"), u.typeOf[Int])))
  assert(im.reflectMethod(ms1 asMethodSymbol)("A", 1) == 1)
  assert(im.reflectMethod(ms2 asMethodSymbol)("A", 1) == 1)
  assert(im.reflectMethod(ms3 asMethodSymbol)(1, "A") == 2)
  assert(im.reflectMethod(ms4 asMethodSymbol)(1, "A") == 2)
}
