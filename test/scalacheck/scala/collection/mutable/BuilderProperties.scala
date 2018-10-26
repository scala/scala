package scala.collection.mutable

class BuilderProperties {

}


import scala.language.higherKinds
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.commands.Commands

import scala.collection.mutable
import scala.util.{Success, Try}

/** Generic stateful property testing for builders
  *
  * Usage: {{{
  *   class MyCollectionProperties extends Properties("my.Collection") {
  *
  *      property("MyCollection builder stateful testing") =
  *        new BuilderStateProperties[K, V, ListMap, MyMap](ListMap, MyMap).property() &&
  *        new BuilderStateProperties[K, V, Map, MyMap](Map, MyMap).property() &&
  *        new BuilderStateProperties[K, V, VectorMap, MyMap](VectorMap, MyMap).property()
  *   }
  * }}}
  *
  * @param controlFactory the "control" MapFactory. This MapFactory is assumed to be correct.
  *                       That is, it is assumed it can produce maps which immutably concatenate properly.
  *                       It's a good idea to cross validate against two or more control factories.
  * @param factory the factory of the Map implementation in question
  *
  * @param tupleGen  gen for the key-values of this map
  * @tparam K type of the Key
  * @tparam V type of the Value
  * @tparam ControlMap the type of map the controlFactory produces
  * @tparam M the type of map under test
  */
class SeqBuilderStateProperties[A: Arbitrary, To <: Seq[A]](newBuilder: => mutable.Builder[A, To])(arbA: Arbitrary[A]) extends Commands {

  override type State = List[A]
  override type Sut = mutable.Builder[A, To]

  override def genInitialState: Gen[State] = Nil

  override def canCreateNewSut(
    newState: State,
    initSuts: scala.Iterable[State],
    runningSuts: scala.Iterable[Sut]) = true

  override def newSut(state: State) = newBuilder.addAll(state)

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State) = state.isEmpty

  override def genCommand(state: State): Gen[Command] = {
    import Gen._
    Gen.oneOf(
      const(Clear),
      const(Result),
      arbInt.arbitrary.map(SizeHint),
      arbA.arbitrary.map(a => AddOne(a)),
      listOf(arbA.arbitrary).map(a => AddAll(a))
    )
  }

  case object Clear extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.clear()
    override def nextState(state: State) = Nil
    override def preCondition(state: State) = true
  }
  case object Result extends Command {
    override type Result = To
    override def postCondition(state: State, result: Try[Result]) = result == Success(state.reverse)
    override def run(sut: Sut) = sut.result()
    override def nextState(state: State) = state
    override def preCondition(state: State) = true
  }
  case class SizeHint(size: Int) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.sizeHint(size)
    override def nextState(state: State) = state
    override def preCondition(state: State) = true
  }
  case class AddOne(elem: A) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.addOne(elem)
    override def nextState(state: State) = elem :: state
    override def preCondition(state: State) = true
  }
  case class AddAll(elems: scala.collection.immutable.Seq[A]) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.addAll(elems)
    override def nextState(state: State) = state.prependedAll(elems)
    override def preCondition(state: State) = true
  }
}
