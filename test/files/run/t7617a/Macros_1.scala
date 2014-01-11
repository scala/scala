import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def getValueImpl[T](c: Context): c.Expr[T] = {
    import c.universe._
    c.Expr[T](Apply(Select(c.prefix.tree, newTermName("getVal")), Nil))
  }
  def setValueImpl[T](c: Context)(value: c.Expr[T]): c.Expr[Unit] = {
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
