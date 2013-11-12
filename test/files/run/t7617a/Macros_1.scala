import scala.reflect.macros.BlackboxContext
import language.experimental.macros

object Macros {
  def getValueImpl[T](c: BlackboxContext): c.Expr[T] = {
    import c.universe._
    c.Expr[T](Apply(Select(c.prefix.tree, newTermName("getVal")), Nil))
  }
  def setValueImpl[T](c: BlackboxContext)(value: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](Apply(Select(c.prefix.tree, newTermName("setVal")), List(value.tree)))
  }
}

object Module {
  private var _val: String = "hello"
  def setVal(value: String): Unit = this._val = value
  def getVal(): String = this._val

  def value: String = macro Macros.getValueImpl[String]
  def value_=(value: String): Unit = macro Macros.setValueImpl[String]
}
