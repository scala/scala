import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    def enclosingName(sym: Symbol): String = {
      sym.name.toString.stripSuffix(termNames.LOCAL_SUFFIX_STRING)
    }
    q"println(${enclosingName(c.internal.enclosingOwner).toString}); 42"
  }

  def foo: Int = macro impl
}