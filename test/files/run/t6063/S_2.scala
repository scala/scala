import java.lang.reflect.Modifier._

object Test {
  def main(args: Array[String]): Unit = {
    val forwarders = Class.forName("foo.Ob").getMethods.toList filter (m => isStatic(m.getModifiers))
    forwarders.sortBy(_.toString) foreach println
  }
}
