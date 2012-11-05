/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import scala.annotation.tailrec
import java.io.EOFException

trait EvalLoop {
  def prompt: String

  def loop(action: (String) => Unit) {
    @tailrec def inner() {
      Console.print(prompt)
      val line = try Console.readLine catch { case _: EOFException => null }
      if (line != null && line != "") {
        action(line)
        inner()
      }
    }
    inner()
  }
}
