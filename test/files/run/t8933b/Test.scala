class MotherClass extends MixinWithSymbol {
  def foo = Symbol("sym1")
}

object Test {
  def main(args: Array[String]): Unit = {
    (new MotherClass).symbolFromTrait
  }
}
