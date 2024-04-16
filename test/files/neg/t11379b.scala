//> using options -Werror -Wvalue-discard
class OneTypeParam[B](value: B) {
  def map[B1](fn: B => B1): OneTypeParam[B1] = new OneTypeParam(fn(value))
  def unitValue: OneTypeParam[Unit] = new OneTypeParam(())
  def checkCompiler: OneTypeParam[Unit] = unitValue.map(_ => unitValue)
}

class TwoTypeParam[A, B](value: B) {
  def map[B1](fn: B => B1): TwoTypeParam[A, B1] = new TwoTypeParam(fn(value))
  def unitValue[C]: TwoTypeParam[C, Unit] = new TwoTypeParam(())
  def checkCompiler: TwoTypeParam[String, Unit] = unitValue.map(_ => unitValue)
}
