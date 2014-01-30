package scala.tools
package reflect

import scala.reflect.{ClassTag, classTag}
import scala.reflect.api.{Mirror, TypeCreator, Universe => ApiUniverse}

// [Eugene++] Before 2.10 is released, I suggest we don't rely on automated type tag generation
// sure, it's convenient, but then refactoring reflection / reification becomes a pain
// `ClassTag` tags are fine, because they don't need a reifier to be generated

trait StdTags {
  val u: ApiUniverse with Singleton
  val m: Mirror[u.type]

  lazy val tagOfListOfString: u.TypeTag[List[String]] =
    u.TypeTag[List[String]](
      m,
      new TypeCreator {
        def apply[U <: ApiUniverse with Singleton](m: Mirror[U]): U # Type = {
          val u = m.universe
          u.appliedType(u.definitions.ListClass.toType, List(u.definitions.StringClass.toType))
        }
      })

  protected def tagOfStaticClass[T: ClassTag]: u.TypeTag[T] =
    u.TypeTag[T](
      m,
      new TypeCreator {
        def apply[U <: ApiUniverse with Singleton](m: Mirror[U]): U # Type =
          m.staticClass(classTag[T].runtimeClass.getName).toTypeConstructor.asInstanceOf[U # Type]
      })
  lazy val tagOfInt = u.TypeTag.Int
  lazy val tagOfString = tagOfStaticClass[String]
  lazy val tagOfFile = tagOfStaticClass[scala.tools.nsc.io.File]
  lazy val tagOfDirectory = tagOfStaticClass[scala.tools.nsc.io.Directory]
  lazy val tagOfThrowable = tagOfStaticClass[java.lang.Throwable]
  lazy val tagOfClassLoader = tagOfStaticClass[java.lang.ClassLoader]
  lazy val tagOfBigInt = tagOfStaticClass[BigInt]
  lazy val tagOfBigDecimal = tagOfStaticClass[BigDecimal]
  lazy val tagOfCalendar = tagOfStaticClass[java.util.Calendar]
  lazy val tagOfDate = tagOfStaticClass[java.util.Date]
}

object StdRuntimeTags extends StdTags {
  val u: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  val m = u.runtimeMirror(getClass.getClassLoader)
  // we need getClass.getClassLoader to support the stuff from scala-compiler.jar
}

abstract class StdContextTags extends StdTags {
  val tc: scala.reflect.macros.contexts.Context
  val u: tc.universe.type = tc.universe
  val m = tc.mirror
}
