abstract class MapLocation(ID: Int) {
  abstract class Message
  case class ReceivePlayer(id: Int) extends Message

  def foo(p: Message): Unit = {
    p match {
      case ReceivePlayer(ID) =>
        ()
    }
  }
}
