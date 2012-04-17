package scala.reflect.makro
package runtime

trait Enclosures {
  self: Context =>

  import mirror._

  // vals are eager to simplify debugging
  // after all we wouldn't save that much time by making them lazy

  val macroApplication: Tree = expandee

  val enclosingMacros: List[Context] = this :: mirror.analyzer.openMacros // include self

  val enclosingImplicits: List[(Type, Tree)] = callsiteTyper.context.openImplicits

  val enclosingPosition: Position = enclosingMacros.find(c => c.macroApplication.pos != NoPosition).map(_.macroApplication.pos).getOrElse(NoPosition)

  val enclosingApplication: Tree = {
    def loop(context: analyzer.Context): Tree = context match {
      case analyzer.NoContext => EmptyTree
      case context if context.tree.isInstanceOf[Apply] => context.tree
      case context => loop(context.outer)
    }

    val context = callsiteTyper.context
    loop(context)
  }

  val enclosingMethod: Tree = callsiteTyper.context.enclMethod.tree

  val enclosingClass: Tree = callsiteTyper.context.enclClass.tree

  val enclosingUnit: CompilationUnit = currentRun.currentUnit
}