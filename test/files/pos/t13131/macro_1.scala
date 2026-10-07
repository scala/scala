//> using jvm 21+
//> using options --release:17

package macros

import scala.reflect.macros.blackbox

object MyClassMacroImpl {

  def derived[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[MyClass[T]] = {
    import c.universe._

    val deprecatedType = typeOf[scala.deprecated]

    val weakType = weakTypeOf[T]
    val annotations = weakType.typeSymbol.annotations

    annotations
      .foreach { ann =>
        val isSubType = ann.tree.tpe <:< deprecatedType
        /*
        import scala.reflect.internal.{AnnotationInfos, SymbolTable}
        val g = c.universe.asInstanceOf[AnnotationInfos with SymbolTable]
        val isSubType = ann.asInstanceOf[g.AnnotationInfo].matches(symbolOf[scala.deprecated].asInstanceOf[g.Symbol])
        */
        assert(!isSubType)
      }

    c.Expr[MyClass[T]](
      q"""_root_.macros.MyClass.apply()"""
    )
  }
}
