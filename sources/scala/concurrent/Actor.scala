package scala.concurrent;

abstract class Actor extends Thread() {

  type Message = Any;

  private val mb = new MailBox;

  def send(msg: Message): unit =
    mb.send(msg);

  protected def receive[a](f: PartialFunction[Message, a]): a =
    mb.receive(f);

  protected def receiveWithin[a](msec: long)(f: PartialFunction[Message, a]): a =
    mb.receiveWithin(msec)(f);
}





