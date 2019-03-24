trait MixinWithSymbol {
  self: MotherClass =>
  def symbolFromTrait: Any = Symbol("traitSymbol")
}
