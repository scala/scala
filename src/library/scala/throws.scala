/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

// $Id$


package scala

/**
 * Annotation for specifying the exceptions thrown by a method.
 * <p>
 *  Example:
 * </p>
 * <pre>
 * <b>class</b> Reader(fname: String) {
 *   <b>private val</b> in =
 *     <b>new</b> BufferedReader(<b>new</b> FileReader(fname))
 *   [throws(classOf[IOException])]
 *   <b>def</b> read() = in.read()
 * }</pre>
 *
 * @author  Nikolay Mihaylov
 * @version 1.0, 19/05/2006
 */
class throws(clazz: java.lang.Class) extends Annotation
