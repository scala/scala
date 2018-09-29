package scala.collection.immutable

import scala.language.higherKinds
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}
import org.scalacheck.commands.Commands

import scala.collection.mutable
import scala.util.{Success, Try}


object SetProperties extends Properties("immutable.Set builder implementations"){

  type A = Int

  property("Set builder stateful testing") = new SetBuilderStateProperties(Set.newBuilder[A]).property()
  // TODO: If and when https://github.com/scala/bug/issues/11160 is fixed, uncomment this
  //  property("BitSet builder stateful testing") =
  //    new SetBuilderStateProperties(BitSet.newBuilder)(arbA = Arbitrary(Gen.choose(0, 10000))).property()
  property("HashSet builder stateful testing") = new SetBuilderStateProperties(HashSet.newBuilder[A]).property()
  property("ListSet builder stateful testing") = new SetBuilderStateProperties(ListSet.newBuilder[A]).property()
  property("SortedSet builder stateful testing") = new SetBuilderStateProperties(SortedSet.newBuilder[A]).property()
  property("TreeSet builder stateful testing") = new SetBuilderStateProperties(TreeSet.newBuilder[A]).property()
}


/** Generic stateful property testing for Set builders
  *
  * Usage: {{{
  *   class MyCollectionProperties extends Properties("my.Collection") {
  *      property("MyCollection builder stateful testing") =
  *        new SetBuilderStateProperties(MySet.newBuilder[A]).property() &&
  *   }
  * }}}
  * @param arbA  gen for the elements of the Set
  * @tparam To the type of Set under test
  */
class SetBuilderStateProperties[A, To <: Set[A]](newBuilder: => mutable.Builder[A, To])(implicit arbA: Arbitrary[A]) extends Commands {

  override type State = Set[A]
  override type Sut = mutable.Builder[A, To]

  override def genInitialState: Gen[State] = Set.empty[A]

  override def canCreateNewSut(newState: State, initSuts: scala.Iterable[State], runningSuts: scala.Iterable[Sut]) = true

  override def newSut(state: State): mutable.Builder[A, To] = newBuilder.addAll(state)

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State) = state.isEmpty

  import Gen._
  lazy val _genCommand = Gen.oneOf(
    const(Clear),
    const(Result),
    choose(0, 10000).map(SizeHint(_)),
    arbA.arbitrary.map(a => AddOne(a)),
    listOf(arbA.arbitrary).map(a => AddAll(a))
  )

  override def genCommand(state: State): Gen[Command] = _genCommand

  override def shrinkState = Shrink.apply[State]( set => set.to(Stream).map(set - _) )

  case object Clear extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.clear()
    override def nextState(state: State) = Set.empty
    override def preCondition(state: State) = true
  }
  case object Result extends Command {
    override type Result = State
    override def postCondition(state: State, result: Try[Result]) = result == Success(state)
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
    override def nextState(state: State) = state + elem
    override def preCondition(state: State) = true
  }
  case class AddAll(elems: scala.collection.immutable.Seq[A]) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.addAll(elems)
    override def nextState(state: State) = state ++ elems
    override def preCondition(state: State) = true
  }
}
