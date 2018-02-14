class MotherClass extends MixinWithSymbol {
  val classSymbol = 'classSymbol
}

object Test {
  def main(args: Array[String]): Unit = {
    val symbol = (new MotherClass).symbolFromTrait
    println(symbol)
  }
}
