/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml
package include

/**
 * <p>
 * <code>XIncludeException</code> is the generic superclass
 * for all checked exceptions that may be thrown as a result
 * of a violation of XInclude's rules.
 * </p>
 * <p>
 * Constructs an <code>XIncludeException</code> with the specified detail
 * message. The error message string <code>message</code> can later be
 * retrieved by the <code>{@link java.lang.Throwable#getMessage}</code>
 * method of class <code>java.lang.Throwable</code>.
 * </p>
 *
 * @param   message   the detail message.
 */
class XIncludeException(message: String) extends Exception(message) {

  /**
   * uses <code>null</code> as its error detail message.
   */
  def this() = this(null)

  private var rootCause: Throwable = null

  /**
   * When an <code>IOException</code>, <code>MalformedURLException</code>
   * or other generic exception is thrown while processing an XML document
   * for XIncludes, it is customarily replaced
   * by some form of <code>XIncludeException</code>.
   * This method allows you to store the original exception.
   *
   * @param   nestedException   the underlying exception which
   caused the XIncludeException to be thrown
   */
  def setRootCause(nestedException: Throwable ) {
    this.rootCause = nestedException
  }

  /**
   * When an <code>IOException</code>, <code>MalformedURLException</code>
   * or other generic exception is thrown while processing an XML document
   * for XIncludes, it is customarily replaced
   * by some form of <code>XIncludeException</code>.
   * This method allows you to retrieve the original exception.
   * It returns null if no such exception caused this <code>XIncludeException</code>.
   *
   * @return Throwable   the underlying exception which caused the
   *                     <code>XIncludeException</code> to be thrown
   */
  def getRootCause(): Throwable = this.rootCause

}
