package scala.tools.util

import scala.tools.nsc.io._
import java.net.URLClassLoader
import scala.collection.JavaConverters._
import scala.language.postfixOps

object VerifyClass {

  // Returns the error if there's a failure
  private def checkClass(name : String, cl: ClassLoader) : (String, Option[String]) = {
    try   {
      Class.forName(name, true, cl)
      (name, None)
    } catch {
      case x: Throwable => // TODO: only catch VerifyError (and related) + ExceptionInInitializationError (for static objects that bomb on classload)
        (name, Some(x.toString))
    }
  }

  def checkClassesInJar(name: String, cl: ClassLoader) = new Jar(File(name)) filter (_.getName.endsWith(".class")) map { x =>
    checkClass(x.getName.stripSuffix(".class").replace('/', '.'), cl)
  } toMap

  def checkClassesInDir(name: String, cl: ClassLoader) = (for {
    file <- Path(name).walk
    if file.name endsWith ".class"
  } yield checkClass(name, cl)) toMap

  def checkClasses(name: String, cl: ClassLoader) =
    if (name endsWith ".jar")  checkClassesInJar(name, cl)
    else checkClassesInDir(name, cl)

  /** Attempts to load all classes on the classpath defined in the args string array.  This method is meant to be used via reflection from tools like SBT or Ant. */
  def run(args: Array[String]): java.util.Map[String, String] = {
    val urls = args.map(Path.apply).map(_.toFile.toURI.toURL).toArray
    println("As urls: " + urls.mkString(","))
    val cl = URLClassLoader.newInstance(urls, null)
    val results = args.flatMap(n => checkClasses(n, cl)).toMap
    (for { (name, result) <- results } yield (name, result.getOrElse(null))).asJava
  }


  def main(args: Array[String]): Unit = {
    val results = run(args).asScala
    println("Processed " + results.size + " classes.")
    val errors = results.filter(_._2 != null)
    for( (name, result) <- results; if result != null) {
      println(name + " had error: " + result)
    }
    System.exit(if(errors.size > 0) 1 else 0)
  }
}
