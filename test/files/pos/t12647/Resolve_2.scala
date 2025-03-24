
//> using options -Xsource:3

import language.experimental.macros

trait Resolver {
  def resolve: Result = ???
}

class ValueResolver extends Resolver {
  override def resolve: Result { def value: String } = valueResult
  def valueResult: Result = macro Macros.impl
}
