class Message(messageType: String, reason: Option[String])

class ReproForSI6921 {

  private[this] var reason = ""

  def decideElection = {
    val explanation = None
    new Message("", reason = explanation)
  }
}
