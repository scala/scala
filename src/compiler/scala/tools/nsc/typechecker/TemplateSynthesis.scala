package scala.tools.nsc
package typechecker

import scala.collection.mutable

trait TemplateSynthesis {
  self: Analyzer =>

  import global._

  private val templatesOf = new mutable.HashMap[Symbol, Template]()
  private val contextsOf = new mutable.HashMap[Symbol, Context]()

  def templateOf(sym: Symbol, orElse: => Template = null): Template = templatesOf.get(sym) match {
    case Some(templ) => templ
    case _ if orElse != null => orElse
    case _ => abort(sym.toString)
  }

  def contextOf(sym: Symbol): Context = contextsOf(sym)

  def rememberTemplate(sym: Symbol, templ: Template): Unit = rememberTemplateAndContext(sym, templ, contextsOf.getOrElse(sym, NoContext))

  def rememberTemplateAndContext(sym: Symbol, templ: Template, context: Context): Unit = {
    val relevantSymbols = List(sym, sym.sourceModule, sym.moduleClass) filter (_ ne NoSymbol)
    relevantSymbols foreach (sym => {
      templatesOf(sym) = templ
      contextsOf(sym) = context
    })
  }
}
