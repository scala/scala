/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect.runtime

import java.lang.{Class => jClass}
import java.lang.reflect.{ Method, InvocationTargetException, UndeclaredThrowableException }
import scala.reflect.internal.util.AbstractFileClassLoader
import scala.reflect.io._

/** A few java-reflection oriented utility functions useful during reflection bootstrapping.
 */
object ReflectionUtils {
  // Unwraps some chained exceptions which arise during reflective calls.
  def unwrapThrowable(x: Throwable): Throwable = x match {
    case  _: InvocationTargetException |      // thrown by reflectively invoked method or constructor
          _: ExceptionInInitializerError |    // thrown when running a static initializer (e.g. a scala module constructor)
          _: UndeclaredThrowableException |   // invocation on a proxy instance if its invocation handler's `invoke` throws an exception
          _: ClassNotFoundException |         // no definition for a class instantiated by name
          _: NoClassDefFoundError             // the definition existed when the executing class was compiled, but can no longer be found
            if x.getCause != null =>
              unwrapThrowable(x.getCause)
    case _ => x
  }
  // Transforms an exception handler into one which will only receive the unwrapped
  // exceptions (for the values of wrap covered in unwrapThrowable.)
  def unwrapHandler[T](pf: PartialFunction[Throwable, T]): PartialFunction[Throwable, T] = {
    case ex if pf isDefinedAt unwrapThrowable(ex)   => pf(unwrapThrowable(ex))
  }

  def show(cl: ClassLoader): String = {
    import scala.language.reflectiveCalls

    def isAbstractFileClassLoader(clazz: Class[_]): Boolean = {
      if (clazz == null) return false
      if (clazz == classOf[AbstractFileClassLoader]) return true
      isAbstractFileClassLoader(clazz.getSuperclass)
    }
    def inferClasspath(cl: ClassLoader): String = cl match {
      case cl: java.net.URLClassLoader =>
        (cl.getURLs mkString ",")
      case cl if cl != null && isAbstractFileClassLoader(cl.getClass) =>
        cl.asInstanceOf[{val root: scala.reflect.io.AbstractFile}].root.canonicalPath
      case null =>
        val loadBootCp = (flavor: String) => scala.util.Properties.propOrNone(flavor + ".boot.class.path")
        loadBootCp("sun") orElse loadBootCp("java") getOrElse "<unknown>"
      case _ =>
        "<unknown>"
    }
    cl match {
      case cl if cl != null =>
        "%s of type %s with classpath [%s] and parent being %s".format(cl, cl.getClass, inferClasspath(cl), show(cl.getParent))
      case null =>
        "primordial classloader with boot classpath [%s]".format(inferClasspath(cl))
    }
  }

  def staticSingletonInstance(cl: ClassLoader, className: String): AnyRef = {
    val name = if (className endsWith "$") className else className + "$"
    val clazz = java.lang.Class.forName(name, true, cl)
    staticSingletonInstance(clazz)
  }

  def staticSingletonInstance(clazz: Class[_]): AnyRef = clazz getField "MODULE$" get null

  def innerSingletonInstance(outer: AnyRef, className: String): AnyRef = {
    val accessorName = if (className endsWith "$") className.substring(0, className.length - 1) else className
    def singletonAccessor(clazz: Class[_]): Option[Method] =
      if (clazz == null) None
      else {
        val declaredAccessor = clazz.getDeclaredMethods.find(_.getName == accessorName)
        declaredAccessor orElse singletonAccessor(clazz.getSuperclass)
      }

    val accessor = singletonAccessor(outer.getClass) getOrElse { throw new NoSuchMethodException(s"${outer.getClass.getName}.$accessorName") }
    accessor setAccessible true
    accessor invoke outer
  }

  object PrimitiveOrArray {
    def unapply(jclazz: jClass[_]) = jclazz.isPrimitive || jclazz.isArray
  }

  class EnclosedIn[T](enclosure: jClass[_] => T) {
    def unapply(jclazz: jClass[_]): Option[T] = Option(enclosure(jclazz))
  }

  object EnclosedInMethod extends EnclosedIn(_.getEnclosingMethod)
  object EnclosedInConstructor extends EnclosedIn(_.getEnclosingConstructor)
  object EnclosedInClass extends EnclosedIn(_.getEnclosingClass)
  object EnclosedInPackage extends EnclosedIn(_.getPackage)

  def associatedFile(clazz: Class[_]): AbstractFile = {
    // TODO: I agree with Jason - this implementation isn't something that we'd like to support
    // therefore I'm having it commented out and this function will now return NoAbstractFile
    // I think we can keep the source code though, because it can be useful to the others
    //
    // def inferAssociatedFile(clazz: Class[_]): AbstractFile = {
    //   // http://stackoverflow.com/questions/227486/find-where-java-class-is-loaded-from
    //   try {
    //     var cl = clazz.getClassLoader()
    //     if (cl == null) {
    //       cl = ClassLoader.getSystemClassLoader()
    //       while (cl != null && cl.getParent != null) cl = cl.getParent
    //     }
    //     var result: AbstractFile = null
    //     if (cl != null) {
    //       val name = clazz.getCanonicalName()
    //       val resource = cl.getResource(name.replace(".", "/") + ".class")
    //       if (resource != null) {
    //         def fromFile(file: String) = AbstractFile.getFile(file)
    //         def fromJarEntry(jarfile: String, entrypath: String) = {
    //           val jar = fromFile(jarfile)
    //           new VirtualFile(clazz.getName, entrypath) {
    //             lazy val impl: AbstractFile = {
    //               def loop(root: AbstractFile, path: List[String]): AbstractFile = {
    //                 def find(name: String) = root.iterator.find(_.name == name).getOrElse(NoAbstractFile)
    //                 path match {
    //                   case step :: Nil => find(step)
    //                   case step :: rest => loop(find(step), rest)
    //                   case Nil => NoAbstractFile
    //                 }
    //               }
    //               loop(ZipArchive.fromFile(new JFile(jarfile)), entrypath.split("/").toList)
    //             }
    //             override def container        = impl.container
    //             override def lastModified     = impl.lastModified
    //             override def input            = impl.input
    //             override def sizeOption       = impl.sizeOption
    //             override def underlyingSource = Some(jar)
    //             override def toString         = jarfile + "(" + entrypath + ")"
    //           }
    //         }
    //         def fallback() = new VirtualFile(clazz.getName, resource.toString)
    //         result = resource.getProtocol match {
    //           case "file" =>
    //             fromFile(resource.getFile)
    //           case "jar" =>
    //             val intrajarUrl = new java.net.URL(resource.getFile)
    //             intrajarUrl.getProtocol match {
    //               case "file" =>
    //                 val file = intrajarUrl.getFile()
    //                 val expectedSuffix = "!/" + name.replace(".", "/") + ".class"
    //                 if (file.endsWith(expectedSuffix)) fromJarEntry(file.stripSuffix(expectedSuffix), expectedSuffix.substring(2))
    //                 else fallback()
    //               case _ => fallback()
    //             }
    //           case _ =>
    //             fallback()
    //         }
    //       }
    //     }
    //     if (result != null) result else NoAbstractFile
    //   } catch {
    //     case _: Exception => NoAbstractFile
    //   }
    // }
    // inferAssociatedFile(clazz)
    NoAbstractFile
  }
}

