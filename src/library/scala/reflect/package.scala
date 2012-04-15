package scala

package object reflect {

  import ReflectionUtils._
  import scala.compat.Platform.EOL

  // !!! This was a val; we can't throw exceptions that aggressively without breaking
  // non-standard environments, e.g. google app engine.  I made it a lazy val, but
  // I think it would be better yet to throw the exception somewhere else - not during
  // initialization, but in response to a doomed attempt to utilize it.

  // todo. default mirror (a static object) might become a source for memory leaks (because it holds a strong reference to a classloader)!
  lazy val mirror: api.Mirror = mkMirror(defaultReflectionClassLoader)

  private def mirrorDiagnostics(cl: ClassLoader): String = """
    |
    | This error has happened because `scala.reflect.runtime.package` located in
    | scala-compiler.jar cannot be loaded. Classloader you are using is:
    | %s.
    |
    | In Scala 2.10.0 M3, scala-compiler.jar is required to be on the classpath
    | for manifests and type tags to function. This will change in the final release,
    | but for now you need to adjust your scripts or build system to proceed.
    | Here are the instructions for some of the situations that might be relevant.
    |
    | If you compile your application directly from the command line
    | or a hand-rolled script, this is a bug. Please, report it.
    |
    | If you compile your application with Maven using the maven-scala plugin,
    | set its "fork" configuration entry to "false:
    |
    |   <plugin>
    |     <groupId>org.scala-tools</groupId>
    |     <artifactId>maven-scala-plugin</artifactId>
    |     <version>2.15.0</version>
    |     <executions>
    |       <execution>
    |         <goals>
    |           ...
    |         </goals>
    |         <configuration>
    |          <fork>false</fork>
    |          ...
    |        </configuration>
    |      </execution>
    |    </executions>
    |  </plugin>
    |
    | If you compile your application with SBT,
    | <to be implemented: release SBT for 2.10.0 M3>
    |
    | If you compile your application in Scala IDE,
    | <to be implemented: release Scala IDE for 2.10.0 M3>.
    |
    | If you launch your application directly from the command line
    | or a hand-rolled script, add `scala-compiler.jar` to the classpath:
    |
    |   scalac HelloWorld.scala
    |   scala HelloWorld -cp path/to/scala-compiler.jar
    |
    | If you launch your application with Maven using the maven-scala plugin,
    | set its "fork" configuration entry to "false as shown above.
    |
    | If you launch your application with SBT, make sure that you use
    | <to be implemented: release SBT for 2.10.0 M3>
    |
    | If you launch your application in Scala IDE, make sure that both scala-library.jar and scala-compiler.jar
    | are in bootstrap entries on the classpath of your launch configuration.
  """.stripMargin('|').format(show(cl))

  def mkMirror(classLoader: ClassLoader): api.Mirror = {
    val coreClassLoader = getClass.getClassLoader
    val instance = invokeFactoryOpt(coreClassLoader, "scala.reflect.runtime.package", "mkMirror", classLoader)
    instance match {
      case Some(x: api.Mirror) => x
      case Some(_) => throw new UnsupportedOperationException("Available scala reflection implementation is incompatible with this interface." + mirrorDiagnostics(coreClassLoader))
      case None => throw new UnsupportedOperationException("Scala reflection not available on this platform." + mirrorDiagnostics(coreClassLoader))
    }
  }

  @deprecated("Use `@scala.beans.BeanDescription` instead", "2.10.0")
  type BeanDescription = scala.beans.BeanDescription
  @deprecated("Use `@scala.beans.BeanDisplayName` instead", "2.10.0")
  type BeanDisplayName = scala.beans.BeanDisplayName
  @deprecated("Use `@scala.beans.BeanInfo` instead", "2.10.0")
  type BeanInfo = scala.beans.BeanInfo
  @deprecated("Use `@scala.beans.BeanInfoSkip` instead", "2.10.0")
  type BeanInfoSkip = scala.beans.BeanInfoSkip
  @deprecated("Use `@scala.beans.BeanProperty` instead", "2.10.0")
  type BeanProperty = scala.beans.BeanProperty
  @deprecated("Use `@scala.beans.BooleanBeanProperty` instead", "2.10.0")
  type BooleanBeanProperty = scala.beans.BooleanBeanProperty
  @deprecated("Use `@scala.beans.ScalaBeanInfo` instead", "2.10.0")
  type ScalaBeanInfo = scala.beans.ScalaBeanInfo

  @deprecated("Use `@scala.reflect.ClassTag` instead", "2.10.0")
  type ClassManifest[T] = ClassTag[T]
  @deprecated("OptManifest is no longer supported, and using it may lead to incorrect results, Use `@scala.reflect.TypeTag` instead", "2.10.0")
  type OptManifest[T]   = TypeTag[T]
  @deprecated("Use `@scala.reflect.ConcreteTypeTag` instead", "2.10.0")
  type Manifest[T]      = ConcreteTypeTag[T]

  @deprecated("Use `@scala.reflect.ClassTag` instead", "2.10.0")
  val ClassManifest     = ClassTag
  @deprecated("Use `@scala.reflect.ConcreteTypeTag` instead", "2.10.0")
  lazy val Manifest     = ConcreteTypeTag
  @deprecated("NoManifest is no longer supported, and using it may lead to incorrect results, Use `@scala.reflect.TypeTag` instead", "2.10.0")
  object NoManifest extends OptManifest[Nothing](scala.reflect.mirror.definitions.NothingClass.asType) with Serializable

  // ClassTag class is defined separately from the mirror
  type TypeTag[T]          = scala.reflect.mirror.TypeTag[T]
  type ConcreteTypeTag[T]  = scala.reflect.mirror.ConcreteTypeTag[T]

  // ClassTag object is defined separately from the mirror
  lazy val TypeTag         = scala.reflect.mirror.TypeTag
  lazy val ConcreteTypeTag = scala.reflect.mirror.ConcreteTypeTag
}
