trait MixinWithSymbol {
  self: MotherClass =>
  def symbolFromTrait: Any = 'traitSymbol
}
