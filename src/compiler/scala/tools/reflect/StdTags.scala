package scala.tools
package reflect

import java.lang.{Class => jClass}
import scala.reflect.{ClassTag, classTag}
import scala.reflect.base.{MirrorOf, TypeCreator, Universe => BaseUniverse}
import scala.reflect.runtime.{universe => ru}

// [Eugene++] Before 2.10 is released, I suggest we don't rely on automated type tag generation
// sure, it's convenient, but then refactoring reflection / reification becomes a pain
// `ClassTag` tags are fine, because they don't need a reifier to be generated

object StdTags {
  // root mirror is fine for these guys, since scala-library.jar is guaranteed to be reachable from the root mirror
  lazy val tagOfString = ru.TypeTag[String](
    ru.rootMirror,
    new TypeCreator {
      def apply[U <: BaseUniverse with Singleton](m: MirrorOf[U]): U # Type = {
        val u = m.universe
        u.definitions.StringClass.asTypeConstructor
      }
    })
  lazy val tagOfListOfString = ru.TypeTag[List[String]](
    ru.rootMirror,
    new TypeCreator {
      def apply[U <: BaseUniverse with Singleton](m: MirrorOf[U]): U # Type = {
        val u = m.universe
        val pre = u.ThisType(m.staticModule("scala.collection.immutable").moduleClass.asInstanceOf[u.Symbol])
        u.TypeRef(pre, u.definitions.ListClass, List(u.definitions.StringClass.asTypeConstructor))
      }
    })

  // root mirror is NOT fine for these guys, hence we use the `currentMirror` trick
  private val ourClassloader = getClass.getClassLoader
  private def tagOfStaticClass[T: ClassTag] =
    ru.TypeTag[T](
      ru.runtimeMirror(ourClassloader),
      new TypeCreator {
        def apply[U <: BaseUniverse with Singleton](m: MirrorOf[U]): U # Type =
          m.staticClass(classTag[T].runtimeClass.getName).asTypeConstructor.asInstanceOf[U # Type]
      })
  lazy val tagOfInt = ru.TypeTag.Int
  lazy val tagOfFile = tagOfStaticClass[scala.tools.nsc.io.File]
  lazy val tagOfDirectory = tagOfStaticClass[scala.tools.nsc.io.Directory]
  lazy val tagOfStdReplVals = tagOfStaticClass[scala.tools.nsc.interpreter.StdReplVals]
  lazy val tagOfIMain = tagOfStaticClass[scala.tools.nsc.interpreter.IMain]
  lazy val tagOfThrowable = tagOfStaticClass[java.lang.Throwable]
  lazy val tagOfClassLoader = tagOfStaticClass[java.lang.ClassLoader]
}
