trait BaseMapper  {
  type M
  def primaryKeyField: MappedField[M]
}

trait Mapper[A] extends BaseMapper {
  type M = A
}

trait IdPK {
  self: BaseMapper =>
  object id extends MappedField[M]
}

trait KeyedMapper[A]
  extends Mapper[A] with BaseMapper

trait MappedField[A]

abstract class A extends KeyedMapper[A] {
  def primaryKeyField: MappedField[A]
}

abstract class B extends Mapper[B] with BaseMapper with IdPK {
  // okay
  // def primaryKeyField: MappedField[M]

  // SOE
  def primaryKeyField: id.type
}

object Test {
  def foo(a: A, b: B) = if (true) a else b // LUB triggers StackOverflowError
}
