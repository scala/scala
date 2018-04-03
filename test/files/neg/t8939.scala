final case class UserId(value: Int)
final case class User(id: Option[UserId], other: Int)
object Test {
  type UId = UserId
  val l: List[User] = List(User(Some(UserId(1)), 2))
  l.find(_.id == UserId(1))
  l.find(_.id == Option(1))
  l.find(_.id.get == new UId(1))
}