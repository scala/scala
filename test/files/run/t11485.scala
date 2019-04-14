import java.lang.reflect.Modifier

trait HaveFinalMethod {
  final def finalMethod: String = "final"
}

class Child extends HaveFinalMethod

object Test {
  def main(args: Array[String]): Unit = {
    val meth = classOf[Child].getMethod("finalMethod")
    assert(meth.isBridge)
    val mods = meth.getModifiers
    assert(!Modifier.isFinal(mods))
  }
}
