object Test {
  class Outer {
    type Member
    def self: Outer { type Member = Outer.this.Member } = this
  }

  val outer = new Outer
  implicitly[outer.type <:< (Outer { type Member = outer.Member })]
}