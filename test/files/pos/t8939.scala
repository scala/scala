final case class UserId(value: Int)
final case class User(id: Option[UserId], other: Int)
object Test {
  val l: List[User] = List(User(Some(UserId(1)), 2))
  l.find(_.id == Option(UserId(1)))
  l.find(_.id.get == UserId(1))
  l.find(_.id.get.value == 1)
}