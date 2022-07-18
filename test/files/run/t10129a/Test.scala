
import scala.reflect.runtime.universe
import scala.util.Try

// [Note] make sure to run each test in a separate JVM process
//        otherwise they'll have a different (corrupt) state of cache that'll affect the other test
object Test extends App {

  private val mirror = universe.runtimeMirror(getClass.getClassLoader)

  private val cyclicTypeClazz = classOf[CyclicType]
  private val cyclicTypeContainerClazz = classOf[CyclicTypeContainer]

  def handleCyclicReferencesThatJavaCanHandle(): Unit = {
    // currently this throws the following exception:
    //   scala.reflect.internal.Symbols$CyclicReference: illegal cyclic reference involving class CyclicType
    mirror.classSymbol(cyclicTypeClazz).info
  }
  handleCyclicReferencesThatJavaCanHandle()
}
