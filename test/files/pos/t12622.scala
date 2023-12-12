trait ScenarioParam {
  type Builder <: Type
}

trait ScenarioParamBuilder

trait Type {
  type Builder <: ScenarioParamBuilder
}

trait Types[H <: ScenarioParam, T <: Type] extends Type {
  type Builder = H#Builder with T#Builder
}

trait Nil extends Type {
  type Builder = ScenarioParamBuilder
}

trait ScenarioTarget {
  type FilterParam <: Type
}

class P1 extends ScenarioParam
class P2 extends ScenarioParam

object someTarget extends ScenarioTarget {
  type FilterParam = Types[P1, Types[P2, Nil]]
}

class WhereClauseBuilder1[T <: ScenarioTarget] {
  type FilterBuilderType = T#FilterParam#Builder
  def m1(f: FilterBuilderType => Any): Any = null
  def m2(f: T#FilterParam#Builder => Any): Any = null
}

object t {
  (null: WhereClauseBuilder1[someTarget.type]).m1(x => null)

  val stabilizer: WhereClauseBuilder1[someTarget.type] = null
  stabilizer.m1(x => null)

  (null: WhereClauseBuilder1[someTarget.type]).m2(x => null)
}