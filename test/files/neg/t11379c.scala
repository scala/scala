//> using options -Werror -Wvalue-discard
class OneTypeParam[B](value: B) {
  def map[B1](fn: B => B1): OneTypeParam[B1] = new OneTypeParam(fn(value))

  def unitValue: OneTypeParam[Unit] = new OneTypeParam(())
  def typedValue[C]: OneTypeParam[Unit] = new OneTypeParam(())

  def checkCompilerUnTyped: OneTypeParam[Unit] = unitValue.map(_ => unitValue)
  def checkCompilerTypedInner: OneTypeParam[Unit] = unitValue.map(_ => typedValue)
  def checkCompilerTypedOuter: OneTypeParam[Unit] = typedValue.map(_ => unitValue)
  def checkCompilerTypedBoth: OneTypeParam[Unit] = typedValue.map(_ => typedValue)
}
