//> using jvm 17+
//> using javaOpt --add-opens java.base/java.lang=ALL-UNNAMED
//> using options -Xsource:3

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.reflect.runtime.*, universe.*
import scala.tools.testkit.AssertUtil.assertNotReachable
import scala.tools.testkit.ReflectUtil.*

// `t8946 reflection subtype does not hold onto Thread`
object Test extends App {
  CanOpener.deworm()
  val reflector = new Thread("scala.reflector") {
    override def run() = typeOf[List[String]] <:< typeOf[Seq[_]]
  }
  val joiner: Thread => Unit = { t =>
    t.start()
    t.join()
  }
  assertNotReachable(reflector, universe)(joiner(reflector))
  // without the fix to use WeakHashMap in ThreadLocalStorage:
  //AssertionError: Root scala.reflect.runtime.JavaUniverse@6ce86ce1 held reference Thread[#23,scala.reflector,5,]
}

// Modules are a can of worms for tools which must tread through arbitrary packages.
object CanOpener {
  def deworm(): Unit = {
    val True = java.lang.Boolean.TRUE
    val unnamed = getClass.getClassLoader.getUnnamedModule
    val method = getMethodAccessible[Module]("implAddExportsOrOpens")
    for (base <- ModuleLayer.boot.findModule("java.base").toScala; pkg <- base.getPackages.asScala)
      try
        // the four-argument form works up through JDK 25
        method.invokeAs[Unit](base, pkg, unnamed, True, True)
      catch {
        case _: java.lang.IllegalArgumentException =>
          // but as of JDK 26, there's a fifth argument
          method.invokeAs[Unit](base, pkg, unnamed, True, True, True)
      }
  }
}
