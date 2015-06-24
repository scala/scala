import language.experimental.macros
import scala.reflect.macros._
import blackbox.Context

object X {

  def classTagOrNull[T](implicit t: reflect.ClassTag[T] = null) = t
  // the failed search for ClassTag[T] does not issue a visible
  // error as we fall back to the default argument. But, the
  // macro engine things we have expanded the macro `materializeClassTag[D]()`
  // to `EmptyTree`, and then attaches a backreference from the expansion
  // to the expandee. This is the `MacroExpansionAttachment` tree attachment.
  def foo[D] = classTagOrNull[D]

  def extractor: Any = macro X.extractorMacro
  def extractorMacro(c: Context): c.Expr[Any] = {
    // Later, in reify, an unrelated use of `EmptyTree` in the AST representing
    // the argument is now treated as a macro expansion which should be rolled
    // back in the tree we reify! This ends up generating a call to `implicitly`
    // which leads to an ambiguous error.
    //
    // Any macro call that expands to EmptyTree could have triggered this problem.
    c.universe.reify(new { def something(data: Any) = ??? })
  }

  // Workarounds:
  //
  // 1. Use quasiquotes rather than `reify`. (But, beware to fully qualify all references, e.g. `_root_.scala.Predef.???`)
  // 2. Avoid failed ClassTag lookups (e.g. in the original bug report, annotate the type argument to `map`)
  // 3. In the macro implementation, just before calling the `reify` macro, you could call another macro
  //
  //      def prepareReify = macro prepareReifyImpl
  //      def prepareReifyImpl(c: Context) = {
  //        val symtab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
  //        symtab.EmptyTree.setAttachments(symtab.NoPosition)
  //      }
  //
  //   To make this visible to the macro implementation, it will need to be compiled in an earlier stage,
  //   e.g a separate SBT sub-project.

}
