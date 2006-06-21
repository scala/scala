package scala.actors.multi

/**
 * @author Philipp Haller
 */
[serializable]abstract class Pid {
  def !(msg: MailBox#Message): unit;

  def link(other: Pid): unit;
  def linkTo(other: Pid): unit; // uni-directional
  def unlink(other: Pid): unit;
  def unlinkFrom(other: Pid): unit; // uni-directional
  def exit(reason: Symbol): unit;
  def exit(from: Pid, reason: Symbol): unit;

  def handleExc(destDesc: ExcHandlerDesc, e: Throwable): unit;

  //def become(clos: Actor => Unit): unit
  //def becomeReceiveLoop(f: PartialFunction[MailBox#Message,unit]): unit
}
