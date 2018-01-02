object Test {
  val f0 = 4
  //f0: Int
  f0: 4

  //final val f1: 4 = 4
  //f1: Int
  //f1: 4

  //final val f1b = 4
  //f1b: Int
  //f1b: 4

  val f2 = () => 4
  //f2: (() => Int)
  f2: (() => 4)

  final val f3 = () => 4
  //f3: (() => Int)
  f3: (() => 4)

  //val f4: () => 4 = () => 4

  def foo[T](f: () => T)(t: T): T = t

  val f5 = foo(() => 4)(4)
  //f5: Int
  f5: 4

  val f6 = foo(() => 4)(5)
  //f6: Int
  f6: 4

  def bar[T <: Singleton](f: () => T)(t: T): T = t

  //final val f7 = bar(() => 4)(4)
  //f7: Int
  //f7: 4

  // found 5, required 4
  val f8 = bar(() => 4)(5)

  val f9 = () => (4, () => 5)
  //f9: (() => (Int, () => Int))
  f9: (() => (4, () => 5))

  //val f10: () => (4, () => 5) = () => (4, () => 5)

  var f11 = 4
  //f11: Int
  f11: 4
  //f11 = 5

  final var f12 = 4
  //f12: Int
  f12: 4
  //f12 = 5

  final var f13: 4 = 4
  //f13: Int
  //f13: 4
  f13 = 5

  //final val one = 1
  //final val compiles: 2 = one + 1

  //final val literalOne: 1 = 1
  //final val alsoCompiles: 2 = literalOne + 1

  //final val recFive : 5 = recFive + 0

  val annot0 = 1: @unchecked
  //annot0: Int
  //annot0: Int @unchecked
  annot0: 1 @unchecked

  //final val annot1 = 1: @unchecked
  //annot1: Int
  //annot1: Int @unchecked
  //annot1: 1
  //annot1: 1 @unchecked
}
