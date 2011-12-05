/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka

import akka.actor.newUuid
import java.net.{ InetAddress, UnknownHostException }

/**
 * Akka base Exception. Each Exception gets:
 * <ul>
 *   <li>a uuid for tracking purposes</li>
 *   <li>toString that includes exception name, message, uuid, and the stacktrace</li>
 * </ul>
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class AkkaException(message: String = "", cause: Throwable = null) extends RuntimeException(message, cause) with Serializable {
  val uuid = "%s_%s".format(AkkaException.hostname, newUuid)

  override lazy val toString =
    "%s: %s\n[%s]\n%s".format(getClass.getName, message, uuid, stackTraceToString)

  def stackTraceToString = {
    val trace = getStackTrace
    val sb = new StringBuffer
    for (i ← 0 until trace.length)
      sb.append("\tat %s\n" format trace(i))
    sb.toString
  }
}

object AkkaException {
  val hostname = try {
    InetAddress.getLocalHost.getHostName
  } catch {
    case e: UnknownHostException => "unknown"
  }
}
