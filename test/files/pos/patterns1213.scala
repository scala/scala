abstract class MapLocation(ID: Int) {
  abstract class Message
  case class ReceivePlayer(id: Int) extends Message

  def foo(p: Message) {
    p match {
      case ReceivePlayer(ID) =>
        ()
    }
  }
}
