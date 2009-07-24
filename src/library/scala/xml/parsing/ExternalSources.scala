/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package parsing

import java.net.URL

import scala.io.Source

/**
 *  @author  Burak Emir
 *  @version 1.0
 */
trait ExternalSources { self: ExternalSources with MarkupParser with MarkupHandler =>

  private def externalSourceFromURL(url: URL): Source = {
    import java.io.{BufferedReader, InputStreamReader}
    val in =
      new BufferedReader(
        new InputStreamReader(
	  url.openStream()))

    //@todo: replace this hack with proper Source implementation

    val str = new StringBuilder()
    var inputLine: String = null

    //while (inputLine = in.readLine()) != null) {
    while ({inputLine = in.readLine(); inputLine} ne null) {
      // Console.println(inputLine)  // DEBUG
      str.append(inputLine)
      str.append('\n')  // readable output
    }
    in.close()

    class MyClass extends Source {

      def newIter = new Iterator[Char] {
        var i = -1
        private val len = str.length-1
        def hasNext = i < len
        def next = { i += 1; str.charAt(i) }
      }

      val iter = newIter

      def reset: Source = new MyClass

      /*override var*/ descr = url.toExternalForm()
    }

    new MyClass
  }

  /** ...
   *
   *  @param systemId ...
   *  @return         ...
   */
  def externalSource(systemId: String): Source = {
    if (systemId startsWith "http:")
      return externalSourceFromURL(new URL(systemId))

    var fileStr = input.descr

    if (input.descr startsWith "file:") {
      fileStr = input.descr.substring(5, input.descr.length)
    } else
      fileStr = fileStr.substring(0,
                                  fileStr.lastIndexOf(java.io.File.separator)+1)
    Source.fromPath(fileStr + systemId)()
  }

}
