package scala.tools.tastytest.internal

import java.lang.reflect.Modifier
import java.io.OutputStream

object Runner {
  def run(name: String, out: OutputStream, err: OutputStream): Unit = {
    val objClass = Class.forName(name, true, getClass.getClassLoader)
    val main     = objClass.getMethod("main", classOf[Array[String]])
    if (!Modifier.isStatic(main.getModifiers))
      throw new NoSuchMethodException(name + ".main is not static")
    Console.withOut(out) {
      Console.withErr(err) {
        main.invoke(null, Array.empty[String])
      }
    }
  }
}
