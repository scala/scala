final case class UserId(value: Int)
final case class User(id: Option[UserId], other: Int)
object Test {
  val l: List[User] = List(User(Some(UserId(1)), 2))
  l.find(_.id == UserId(1))
}