class MotherClass

trait MixinWithSymbol {
  self: MotherClass =>
  def symbolFromTrait: Symbol = Symbol("traitSymbol")
}
