package scala.reflect.internal

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.runtime.universe
import scala.util.Try

// [Note] make sure to run each test in a separate JVM process
//        otherwise they'll have a different (corrupt) state of cache that'll affect the other test
@RunWith(classOf[JUnit4])
class CyclicReferenceTest {

  private val mirror = universe.runtimeMirror(getClass.getClassLoader)

  private val cyclicTypeClazz = classOf[CyclicType]
  private val cyclicTypeContainerClazz = classOf[CyclicTypeContainer]

  @Test def handleCyclicReferencesThatJavaCanHandle(): Unit = {
    // currently this throws the following exception:
    //   scala.reflect.internal.Symbols$CyclicReference: illegal cyclic reference involving class CyclicType
    mirror.classSymbol(cyclicTypeClazz).info
  }

  @Test def consistentlyEitherSuccessOrFail(): Unit = {
    // currently the first try fails with
    //   scala.reflect.internal.Symbols$CyclicReference: illegal cyclic reference involving class CyclicType
    // and the second try succeeds with
    //   Scope{}

    val tryMethods1 = Try {
      mirror.classSymbol(cyclicTypeContainerClazz).info.decls
    }

    val tryMethods2 = Try {
      mirror.classSymbol(cyclicTypeContainerClazz).info.decls
    }

    assert(tryMethods1 == tryMethods2, s"First try was ${tryMethods1} but second try was ${tryMethods2}.")
  }

}
