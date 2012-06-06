package scala.tools
package reflect

import java.lang.{Class => jClass}
import scala.reflect.mirror._

// [Eugene++] Before 2.10 is released, I suggest we don't rely on automated type tag generation
// sure, it's convenient, but then refactoring reflection / reification becomes a pain
// `ClassTag` tags are fine, because they don't need a reifier to be generated

object StdTags {
  lazy val tagOfString = TypeTag.String
  lazy val tagOfListOfString = TypeTag[List[String]]({
    val pre = ThisType(staticModule("scala.collection.immutable").moduleClass)
    TypeRef(pre, definitions.ListClass, List(definitions.StringClass.asTypeConstructor))
  }, classOf[List[String]])

  private def tagOfStaticClass[T: ClassTag] = TypeTag[T](staticClass(classTag[T].erasure.getName).asTypeConstructor, classTag[T].erasure)
  lazy val tagOfInt = TypeTag.Int
  lazy val tagOfFile = tagOfStaticClass[scala.tools.nsc.io.File]
  lazy val tagOfDirectory = tagOfStaticClass[scala.tools.nsc.io.Directory]
  lazy val tagOfStdReplVals = tagOfStaticClass[scala.tools.nsc.interpreter.StdReplVals]
  lazy val tagOfIMain = tagOfStaticClass[scala.tools.nsc.interpreter.IMain]
  lazy val tagOfThrowable = tagOfStaticClass[java.lang.Throwable]
  lazy val tagOfClassLoader = tagOfStaticClass[java.lang.ClassLoader]
}
