package tastytest

case class CaseClassDefault(value: Int = 23)

object CaseClassDefault {

  class Inner {
    case class Local(value: Int = 47)
  }

  class FakeCaseClass(val value: Int = 47)
  object FakeCaseClass {
    def apply(value: Int = 97): FakeCaseClass = new FakeCaseClass(value)
  }

}
