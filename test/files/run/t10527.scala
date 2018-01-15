import java.lang.Deprecated

trait Fruit
class Apple extends Fruit

abstract class FruitFactory {
  def getFruit: Fruit
}

class AppleFactory extends FruitFactory {
  @Deprecated def getFruit: Apple = new Apple
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = classOf[AppleFactory]
    val m = c.getDeclaredMethods.filter(_.isBridge)(0)
    val a = m.getAnnotations()(0).annotationType.getName
    assert(a == "java.lang.Deprecated")
  }
}
