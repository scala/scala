/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.doc.html.page

import JSONFormat.format

private[page] object JSONFormat {

  def format(x: Any): String = x match {
    case s:  String     => s"""\"${quoteString(s)}\""""
    case jo: JSONObject => jo.toString
    case ja: JSONArray  => ja.toString
    case other          => throw new UnsupportedOperationException(s"Value $other of class ${other.getClass} cannot be formatted.")
  }

  /** This function can be used to properly quote Strings for JSON output. */
  def quoteString(s: String): String = {
    val len: Int = s.length
    val buf = new StringBuilder(len + len/4)
    var i: Int = 0
    while (i < len) {
      s.apply(i) match {
        case '"'  => buf ++= "\\\""
        case '\\' => buf ++= "\\\\"
        case '/'  => buf ++= "\\/"
        case '\b' => buf ++= "\\b"
        case '\f' => buf ++= "\\f"
        case '\n' => buf ++= "\\n"
        case '\r' => buf ++= "\\r"
        case '\t' => buf ++= "\\t"
        /* We'll unicode escape any control characters. These include:
         * 0x00 -> 0x1f : ASCII Control (C0 Control Codes)
         * 0x7f         : ASCII DELETE
         * 0x80 -> 0x9f : C1 Control Codes
         *
         * Per RFC4627, section 2.5, we're not technically required to
         * encode the C1 codes, but we do to be safe.
         */
        case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) =>
                     val cint = c.toInt
                     buf ++= f"\\u$cint%04x"
        case c    => buf += c
      }
      i += 1
    }
    buf.toString()
  }
}

/** Represents a JSON Object (map). */
private[page] case class JSONObject(obj: Map[String,Any]) {
  override def toString = obj.map({ case (k,v) => format(k) + " : " + format(v) }).mkString("{", ", ", "}")
}

/** Represents a JSON Array (vector). */
private[page] case class JSONArray(vector: Vector[Any]) {
  override def toString = vector.map(format).mkString("[", ", ", "]")
}
