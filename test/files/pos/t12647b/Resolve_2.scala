
import language.experimental.macros

abstract class Resolver {
  def resolve: Result = ???
}

class ValueResolver extends Resolver {
  override def resolve = valueResult
  def valueResult: Result = macro Macros.impl
}
