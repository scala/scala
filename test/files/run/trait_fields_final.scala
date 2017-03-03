// TODO: clarify meaning of final in traits
// In the new compiler, there's no final modifier after mixin for `meh`'s setter,
// whereas 2.12.0-M3 makes meh's trait setter final.
// NOTE: bytecode is identical, but the scalasignature is different
trait Foo { self: Meh =>
  def bar(x: String) = x == "a"
  private final val meh = bar("a")
}

abstract class Meh extends Foo

object Test {
  def main(args: Array[String]): Unit = {
    val setter = classOf[Meh].getDeclaredMethod("Foo$_setter_$Foo$$meh_$eq", java.lang.Boolean.TYPE)
    val getter = classOf[Meh].getDeclaredMethod("Foo$$meh")
    import java.lang.reflect.Modifier._
    assert(isFinal(setter.getModifiers), setter)
    assert(isFinal(getter.getModifiers), getter)
  }

}
