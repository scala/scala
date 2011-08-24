class MailBox {
 class Message
 //type Message = AnyRef
}

abstract class Actor {
 private val in = new MailBox

 def send(msg: in.Message) =  error("foo")

 def unstable: Actor = error("foo")

 def dubiousSend(msg: MailBox#Message): Nothing =
   unstable.send(msg) // in.Message becomes unstable.Message, but that's ok since Message is a concrete type member
}
