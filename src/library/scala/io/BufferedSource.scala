/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io

import java.io.{ InputStream, BufferedReader, InputStreamReader }
import Source.DefaultBufSize

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir, Paul Phillips
 */
class BufferedSource(inputStream: InputStream)(implicit codec: Codec = Codec.default) extends Source
{
  def reader() = new InputStreamReader(inputStream, codec.decoder)
  def bufferedReader() = new BufferedReader(reader(), DefaultBufSize)

  override val iter = {
    val reader = bufferedReader()
    Iterator continually (codec wrap reader.read()) takeWhile (_ != -1) map (_.toChar)
  }
}

