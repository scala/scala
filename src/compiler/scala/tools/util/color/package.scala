/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.util

/**
 *  Wrappers around ansi colors.
 *
 *  @author  Paul Phillips
 *  @version 2.10
 */
package object color {
  implicit def implicitLiftAnsiAtom(c: AnsiAtom): Ansi = new Ansi(List(c))
  implicit def implicitColorToBackground(c: AnsiColor): AnsiBackground = c match {
    case x: AnsiBackground  => x
    case x: AnsiForeground  => x.flip
  }
  implicit def implicitCStringOps(str: String): CStringOps = new CStringOps(str)
  implicit def implicitCString(str: String): CString = new CString(str, str)
}
