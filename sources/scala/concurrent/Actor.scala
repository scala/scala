package scala.concurrent;

abstract class Actor extends Thread() {

  type Message = AnyRef;

  private val mb = new MailBox;

  def send(msg: Message): unit =
    mb.send(msg);

  def receive[a](f: PartialFunction[Message, a]): a =
    mb.receive(f);

  def receiveWithin[a](msec: long)(f: PartialFunction[Message, a]): a =
    mb.receiveWithin(msec)(f);
}





