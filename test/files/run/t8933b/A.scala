trait MixinWithSymbol {
  self: MotherClass =>
  def symbolFromTrait: Any = sym"traitSymbol"
}
