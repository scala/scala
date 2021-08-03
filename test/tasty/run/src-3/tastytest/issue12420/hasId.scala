package tastytest.issue12420

trait HasId[+K] {
  def id: K
}

trait Id[+T, K] {
  def id: K
}

case class UserId(id: String) extends Id[User, String]
case class User(id: UserId) extends HasId[UserId]

class Bar[A <: HasId[Id[A, String]]](val bar: A)
class Foo extends Bar(User(UserId("Foo")))
