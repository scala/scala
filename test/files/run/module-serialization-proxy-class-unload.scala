//> using options -Ddummy=run-in-forked-process
object Module {
  val data = new Array[Byte](32 * 1024 * 1024)
}

object Test {
  private val readResolve = classOf[scala.runtime.ModuleSerializationProxy].getDeclaredMethod("readResolve")
  readResolve.setAccessible(true)

  val testClassesDir = System.getProperty("partest.output")
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 256) {
      // This would "java.lang.OutOfMemoryError: Java heap space" if ModuleSerializationProxy
      // prevented class unloading.
      deserializeDynamicLoadedClass()
    }
  }

  def deserializeDynamicLoadedClass(): Unit = {
    val loader = new java.net.URLClassLoader(Array(new java.io.File(testClassesDir).toURI.toURL), ClassLoader.getSystemClassLoader)
    val moduleClass = loader.loadClass("Module$")
    assert(moduleClass ne Module.getClass)
    val result = readResolve.invoke(new scala.runtime.ModuleSerializationProxy(moduleClass))
    assert(result.getClass == moduleClass)
  }
}
