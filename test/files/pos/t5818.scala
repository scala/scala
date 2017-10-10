abstract class Abstract {
  type TypeMember
  val member: TypeMember
}

object Abstract {
  class Ops(m: Abstract#TypeMember) {
    def answer = 42
  }

  implicit def member2AbstractOps(m: Abstract#TypeMember) = new Ops(m)
}

object ShouldThisCompile {
  val concrete: Abstract = new Abstract {
    type TypeMember = String
    val member = "hello"
  }

  concrete.member.answer
}
