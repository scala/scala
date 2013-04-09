object AdaptWithWeaklyConformantType {
  implicit class D(d: Double) { def double = d*2 }

  val x1: Int = 1
  var x2: Int = 2
  val x3 = 3
  var x4 = 4
  final val x5 = 5
  final var x6 = 6

  def f1 = x1.double
  def f2 = x2.double
  def f3 = x3.double
  def f4 = x4.double
  def f5 = x5.double
  def f6 = x6.double
}

object AdaptAliasWithWeaklyConformantType {
  implicit class D(d: Double) { def double = d*2 }
  type T = Int

  val x1: T = 1
  var x2: T = 2
  val x3 = (3: T)
  var x4 = (4: T)
  final val x5 = (5: T)
  final var x6 = (6: T)

  def f1 = x1.double
  def f2 = x2.double
  def f3 = x3.double
  def f4 = x4.double
  def f5 = x5.double
  def f6 = x6.double
}

object AdaptToAliasWithWeaklyConformantType {
  type U = Double
  implicit class D(d: U) { def double = d*2 }

  val x1: Int = 1
  var x2: Int = 2
  val x3 = (3: Int)
  var x4 = (4: Int)
  final val x5 = (5: Int)
  final var x6 = (6: Int)

  def f1 = x1.double
  def f2 = x2.double
  def f3 = x3.double
  def f4 = x4.double
  def f5 = x5.double
  def f6 = x6.double
}

object AdaptAliasToAliasWithWeaklyConformantType {
  type U = Double
  type T = Int
  implicit class D(d: U) { def double = d*2 }

  val x1: T = 1
  var x2: T = 2
  val x3 = (3: T)
  var x4 = (4: T)
  final val x5 = (5: T)
  final var x6 = (6: T)

  def f1 = x1.double
  def f2 = x2.double
  def f3 = x3.double
  def f4 = x4.double
  def f5 = x5.double
  def f6 = x6.double
}
