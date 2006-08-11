/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.single

import scala.collection.mutable.Queue

/**
 * @author Philipp Haller
 */
trait MailBox {
  /** Unconsumed messages. */
  var sent = new Queue[Any]

  var continuation: PartialFunction[Any,Unit] = null
  // more complex continuation
  var contCases: PartialFunction[Any,Any] = null
  var contThen: Any => unit = null

  def hasCont =
    if ((continuation == null) && (contCases == null)) false
    else true

  def contDefinedAt(msg: Any) =
    if (((continuation != null) && continuation.isDefinedAt(msg)) ||
        ((contCases != null) && contCases.isDefinedAt(msg)))
      true
    else
      false

  var isAlive = true

  private var duration: Long = 0
  private var timeInitial: Long = 0
  private var timeoutEnabled: Boolean = false

  def send(msg: Any): Unit = synchronized {
    if (isAlive)
      if (!hasCont) {
        Debug.info("no cont avail/task already scheduled. appending msg to mailbox.")
        sent += msg
      }
      else {
        var message = msg
        var timeoutOccurred = false

        if (timeoutEnabled && (System.currentTimeMillis() - timeInitial > duration))
          timeoutOccurred = true

        if (timeoutOccurred && !contDefinedAt(TIMEOUT))
          die()
        else {
          if (timeoutOccurred) message = TIMEOUT

          if (contDefinedAt(message)) {
            // we exit receive, so reset timeoutEnabled
            timeoutEnabled = false

            try {
              if (continuation != null) {
                val f = continuation
                continuation = null
                f(msg)
                die()
              }
              else {
                // use more complex receive-and-return continuation
                val cases = contCases
                val then = contThen
                contCases = null
                contThen = null
                val result = cases(msg)
                then(result)
                die()
              }
            }
            catch {
              case d: Done =>
                // do nothing (continuation is already saved)
            }
          }
          else {
            Debug.info("cont not defined at msg. appending to mailbox.")
            if (!timeoutOccurred) sent += message
          }
        }
      }
  }

  def receive(f: PartialFunction[Any, Unit]): Nothing = {
    continuation = null
    sent.dequeueFirst(f.isDefinedAt) match {
      case Some(msg) =>
	f(msg)
        die()
      case None =>
        continuation = f
        Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
    }
    throw new Done
  }

  def receiveWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    timeInitial = System.currentTimeMillis()
    duration = msec

    continuation = null
    sent.dequeueFirst(f.isDefinedAt) match {
      case Some(msg) =>
	f(msg)
        die()
      case None =>
        // if timeout == 0 then execute timeout action if specified (see Erlang book)
        if (duration == 0) {
          if (f.isDefinedAt(TIMEOUT))
            f(TIMEOUT)
          die()
        }
        else {
          timeoutEnabled = true
          continuation = f
          Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
        }
    }
    throw new Done
  }

  def receiveAndReturn(cases: PartialFunction[Any, Any], then: Any => Unit): Nothing = {
    contCases = null
    contThen = null
    sent.dequeueFirst(cases.isDefinedAt) match {
      case Some(msg) => {
        val result = cases(msg)
        then(result)
        die()
      }
      case None => {
        contCases = cases
        contThen = then
        Debug.info("No msg found. Saved complex continuation.")
      }
    }
    throw new Done
  }

  def die() = {
    isAlive = false
    Debug.info("" + this + " died.")
  }
}
