abstract class MapLocation(ID:int) {
  abstract class Message
  case class ReceivePlayer(id: int) extends Message

  def foo(p: Message) {
    p match {
      case ReceivePlayer(ID) =>
        ()
    }
  }
}
