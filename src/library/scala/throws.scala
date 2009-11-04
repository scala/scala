/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** <p>
 *    Annotation for specifying the exceptions thrown by a method.
 *    For example:
 *  </p><pre>
 *    <b>class</b> Reader(fname: String) {
 *      <b>private val</b> in =
 *        <b>new</b> BufferedReader(<b>new</b> <a class="java/io/FileReader" href="" target="_top">FileReader</a>(fname))
 *      @throws(classOf[<a class="java/io/IOException" href="" target="_top">IOException</a>])
 *      <b>def</b> read() = in.read()
 *    }</pre>
 *
 * @author  Nikolay Mihaylov
 * @version 1.0, 19/05/2006
 * @since   2.1
 */
class throws(clazz: Class[_]) extends StaticAnnotation
