/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** <p>
 *    Annotation for specifying the exceptions thrown by a method.
 *    For example:
 * {{{
 * class Reader(fname: String) {
 *   private val in = new BufferedReader(new FileReader(fname))
 *   @throws(classOf[IOException])
 *   def read() = in.read()
 * }
 * }}}
 *
 * @author  Nikolay Mihaylov
 * @version 1.0, 19/05/2006
 * @since   2.1
 */
class throws(clazz: Class[_]) extends annotation.StaticAnnotation
