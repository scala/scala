import scala.reflect.macros.Context

object Impls {
  def parent(c: Context)(x: c.Tree) = c.universe.reify(println("parent"))
  def child(c: Context)(x: c.Tree) = c.universe.reify(println("child"))
}

trait Parent {
  def foo(x: _) = macro Impls.parent
  def bar(x: _): _ = macro Impls.parent
}

trait Child extends Parent {
  override def foo(x: _) = macro Impls.child
  override def bar(x: _): _ = macro Impls.child
}
