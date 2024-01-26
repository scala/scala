package scala.tools.nsc.backend.jvm

import org.junit.Assert.assertEquals
import org.junit.Test

import java.lang.reflect.Member

class ClassfileParserTest {
  @Test
  def noConstantPoolLag(): Unit = {
    def constNames(ms: List[Member]) = ms.collect {
      case f if f.getName.startsWith("CONSTANT_") => f.getName
    }.sorted

    val toScalac = Map(
      "CONSTANT_INTERFACE_METHODREF" -> "CONSTANT_INTFMETHODREF",
      "CONSTANT_INVOKE_DYNAMIC" -> "CONSTANT_INVOKEDYNAMIC",
      "CONSTANT_METHOD_HANDLE" -> "CONSTANT_METHODHANDLE",
      "CONSTANT_METHOD_TYPE" -> "CONSTANT_METHODTYPE",
      "CONSTANT_NAME_AND_TYPE" -> "CONSTANT_NAMEANDTYPE",
    ).withDefault(x => x)

    val asmConsts = constNames(Class.forName("scala.tools.asm.Symbol").getDeclaredFields.toList)
      .map(_.stripSuffix("_TAG"))
      .map(toScalac)
      .::("CONSTANT_UNICODE")
      .sorted
    val scalacConsts = constNames(scala.reflect.internal.ClassfileConstants.getClass.getDeclaredMethods.toList)
    assertEquals(scalacConsts, asmConsts)
  }
}
