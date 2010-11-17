/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import sun.misc.{ Signal, SignalHandler }
import SignalHandler._
import nsc.io.timer

/** Unofficial signal handling code.  According to sun it's unsupported,
 *  but it's too useful not to take advantage of.  Degrade gracefully.
 */
class SignalManager {
  def apply(name: String): SignalWrapper =
    try   { new SignalWrapper(new Signal(name)) }
    catch { case x: IllegalArgumentException => new SignalError(x.getMessage) }

  class ChainedHandler(prev: SignalHandler, current: SignalHandler) extends SignalHandler {
    def handle(sig: Signal): Unit = {
      current handle sig
      if (prev != SIG_DFL && prev != SIG_IGN)
        prev handle sig
    }
  }
  class SignalWrapper(val signal: Signal) {
    def name = signal.getName
    def add(body: => Unit) = {
      val handler = new SignalHandler { def handle(sig: Signal) = body }
      val prev    = Signal.handle(signal, handler)

      new ChainedHandler(prev, handler)
    }
    override def toString = "SIG" + name
  }
  class SignalError(message: String) extends SignalWrapper(null) {
    override def toString = message
  }
}

object SignalManager extends SignalManager {
  private implicit def mkSignalWrapper(name: String): SignalWrapper = this(name)

  def HUP: SignalWrapper    = "HUP"
  def INT: SignalWrapper    = "INT"
  def QUIT: SignalWrapper   = "QUIT"
  def ILL: SignalWrapper    = "ILL"
  def TRAP: SignalWrapper   = "TRAP"
  def ABRT: SignalWrapper   = "ABRT"
  def EMT: SignalWrapper    = "EMT"
  def FPE: SignalWrapper    = "FPE"
  def KILL: SignalWrapper   = "KILL"
  def BUS: SignalWrapper    = "BUS"
  def SEGV: SignalWrapper   = "SEGV"
  def SYS: SignalWrapper    = "SYS"
  def PIPE: SignalWrapper   = "PIPE"
  def ALRM: SignalWrapper   = "ALRM"
  def TERM: SignalWrapper   = "TERM"
  def URG: SignalWrapper    = "URG"
  def STOP: SignalWrapper   = "STOP"
  def TSTP: SignalWrapper   = "TSTP"
  def CONT: SignalWrapper   = "CONT"
  def CHLD: SignalWrapper   = "CHLD"
  def TTIN: SignalWrapper   = "TTIN"
  def TTOU: SignalWrapper   = "TTOU"
  def IO: SignalWrapper     = "IO"
  def XCPU: SignalWrapper   = "XCPU"
  def XFSZ: SignalWrapper   = "XFSZ"
  def VTALRM: SignalWrapper = "VTALRM"
  def PROF: SignalWrapper   = "PROF"
  def WINCH: SignalWrapper  = "WINCH"
  def INFO: SignalWrapper   = "INFO"
  def USR1: SignalWrapper   = "USR1"
  def USR2: SignalWrapper   = "USR2"

  /** Given a number of seconds, a signal, and a function: sets up a handler which upon
   *  receiving the signal once, calls the function with argument true, and if the
   *  signal is received again within the allowed time, calls it with argument false.
   *  (Otherwise it calls it with true and starts the timer over again.)
   */
  def requireInterval(seconds: Int, wrapper: SignalWrapper)(fn: Boolean => Unit) = {
    var received = false
    wrapper add {
      if (received) fn(false)
      else {
        received = true
        fn(true)
        timer(seconds)(received = false)
      }
    }
  }
}



