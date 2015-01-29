class MotherClass extends MixinWithSymbol {
  def foo = 'sym1
}

object Test {
  def main(args: Array[String]) {
    (new MotherClass).symbolFromTrait
  }
}
