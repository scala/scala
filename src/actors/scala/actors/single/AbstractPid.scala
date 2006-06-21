package scala.actors.single;

/**
 * @author Philipp Haller
 */
trait AbstractPid {
  def !(msg: MailBox#Message): unit
  def become(clos: Actor => Unit): unit
  def becomeReceiveLoop(f: PartialFunction[MailBox#Message,unit]): unit
}
