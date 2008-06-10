package script

import java.lang.reflect._

object Keys extends Application {
  generate()

  def generate() = {
    val fields = classOf[java.awt.event.KeyEvent].getFields
    val prefix = "VK_"
    val lines = for (f <- fields; if ((f.getModifiers & Modifier.STATIC) != 0) && f.getName.startsWith(prefix)) yield {
      val id = f.getInt(null)
      val name = javaConstantName2Scala(prefix, f.getName, "Key")
      "val " + name + " = Value(" + f.getName + ")"
    }
    val source = new StringBuilder
    source.append("object Key extends Enumeration {\n")
    source.append("  import java.awt.event.KeyEvent._\n\n")
    lines.foreach(l => source.append("  " + l + "\n"))
    source.append("}")
    println(source)
  }

  def javaConstantName2Scala(prefix: String, name: String, prefixForNumber: String): String = {
    var lower = false
    var last = ' '
    val builder = new StringBuilder
    val trimmed = name.substring(prefix.length)
    val n = if (trimmed(0).isDigit) prefixForNumber + trimmed else trimmed
    for (val c <- n) {
      if (c == '_') {
        lower = false
      }
      else if (lower == true) {
        builder.append(c.toLowerCase)
      }
      else {
        lower = true
        builder.append(c)
      }
    }
    builder.toString
  }
}
