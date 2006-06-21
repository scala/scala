package scala.actors.multi;

/**
 * @author Philipp Haller
 */
class ReceiverTask(val actor: MailBox, msg: MailBox#Message) extends Runnable {
  def run(): unit = {
    try {
      actor receiveMsg msg
    }
    catch {
      case d: Done =>
        // do nothing (continuation is already saved)
    }
  }
}
