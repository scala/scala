class MailBox {
  //class Message
  type Message = AnyRef
}

abstract class Actor {
  private val in = new MailBox

  def send(msg: in.Message) =  sys.error("foo")

  def unstable: Actor = sys.error("foo")

  def dubiousSend(msg: MailBox#Message) =
    unstable.send(msg) // in.Message becomes unstable.Message, but that's ok since Message is a concrete type member
}
