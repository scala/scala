/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.ant.sabbus

import java.net.URL

object Compilers extends scala.collection.DefaultMap[String, Compiler] {

  val debug = false

  private val container = new scala.collection.mutable.HashMap[String, Compiler]

  def iterator = container.iterator

  def get(id: String) = container.get(id)

  override def size = container.size

  def make(id: String, classpath: Array[URL], settings: Settings): Compiler = {
    if (debug) println("Making compiler " + id)
    if (debug) println("  memory before: " + freeMemoryString)
    val comp = new Compiler(classpath, settings)
    container(id) = comp
    if (debug) println("  memory after: " + freeMemoryString)
    comp
  }

  def break(id: String): Null = {
    if (debug) println("Breaking compiler " + id)
    if (debug) println("  memory before: " + freeMemoryString)
    container -= id
    System.gc()
    if (debug) println("  memory after: " + freeMemoryString)
    null
  }

  private def freeMemoryString: String =
    (Runtime.getRuntime.freeMemory/1048576.0).formatted("%10.2f") + " MB"
}
