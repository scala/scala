// interesting hierarchies/overrides distilled from reflect/compiler

trait Aliases {
  val TypeTag = "universe.TypeTag"
}
trait AliasesOverrides extends Aliases { // or self: Aliases =>
  override val TypeTag = "the real universe.TypeTag"
}
class Context extends Aliases with AliasesOverrides



trait SymbolTable {
  def currentRunId: Int = -1
}
trait ReflectSetup extends SymbolTable {
  override val currentRunId = 1
}
class G extends SymbolTable with ReflectSetup


object Test extends App {
  println((new Context).TypeTag)
  println((new G).currentRunId)
}