final case class UserId(value: Int)
final case class User(id: Option[UserId], other: Int)
final case class OtherUser(id: Option[UserId], other: Int)
object Test {
  type UId = UserId
  val l: List[User] = List(User(Some(UserId(1)), 2))
  l.find(_.id == UserId(1))
  l.find(_.id == Option(1))
  l.find(_.id.get == new UId(1))
  val c1 = OtherUser(Some(UserId(1)), 2) == UserId(3)
  val c2 = OtherUser(Some(UserId(1)), 2) == User(Some(UserId(1)), 2)
  val c3 = Some(OtherUser(Some(UserId(1)), 2)) == Some(User(Some(UserId(1)), 2))
  val c4 = (OtherUser(Some(UserId(1)), 2): Any) == User(Some(UserId(1)), 2)
  val c5 = true == 1L
}