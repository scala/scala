/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.collection.mutable.{ArrayBuffer,Buffer}

/**
 * @author Philipp Haller
 */
object Util {
  def pad(s: String, req: int): String = {
    val buf = new StringBuffer
    val add: int = req - s.length()
    for (val i <- List.range(1, add+1))
      buf append "0";
    buf append s
    buf.toString()
  }

  def encode(i: Int) = pad(Integer.toHexString(i), 8)
  def encode(l: Long) = pad(java.lang.Long.toHexString(l), 16)
  def decode(s: String): Int = Integer.decode("0x" + s).intValue()
  def decodeLong(s: String): Long = java.lang.Long.decode("0x" + s).longValue()

  def baseName(o: Any) = {
    val s = o.toString()

    def baseName(s: String): String = {
      if (s.indexOf('$') != -1)
        baseName(s.substring(0,s.indexOf('$')))
      else if (s.indexOf('(') != -1)
        baseName(s.substring(0,s.indexOf('(')))
      else if (s.indexOf('@') != -1)
        baseName(s.substring(0,s.indexOf('@')))
      else s
    }

    baseName(s)
  }

  def extractArgs(s: String): Buffer[String] = {
    // extract strings between first-level commas
    var level: int = 0;
    val carr: Array[char] = s.toCharArray();
    var buf = new StringBuffer; // current string
    val args = new ArrayBuffer[String];

    for (val i <- List.range(0,carr.length)) {
      if ((level == 0) && (carr(i) == ',')) {
        // argument finished
        args += buf.toString();
        buf = new StringBuffer
      } else {
        if (carr(i) == '(') level = level + 1;
        if (carr(i) == ')') level = level - 1;
        buf append carr(i)
      }
    }
    args += buf.toString();
    args
  }
}
