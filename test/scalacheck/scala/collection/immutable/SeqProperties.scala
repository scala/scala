package scala.collection.immutable


import scala.language.higherKinds
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.commands.Commands

import scala.collection.mutable
import scala.util.{Success, Try}
import org.scalacheck.Properties


object SeqProperties extends Properties("immutable.Seq builder implementations"){

  type A = Int

  property("Seq builder stateful testing") = new SeqBuilderStateProperties(Seq.newBuilder[A]).property()
  property("List builder stateful testing") = new SeqBuilderStateProperties(List.newBuilder[A]).property()
  property("ArraySeq builder stateful testing") = new SeqBuilderStateProperties(ArraySeq.newBuilder[A]).property()
  property("LazyList builder stateful testing") = new SeqBuilderStateProperties(LazyList.newBuilder[A]).property()
  property("Queue builder stateful testing") = new SeqBuilderStateProperties(Queue.newBuilder[A]).property()
  property("IndexedSeq builder stateful testing") = new SeqBuilderStateProperties(IndexedSeq.newBuilder[A]).property()
  property("Stream builder stateful testing") = new SeqBuilderStateProperties(Stream.newBuilder[A]).property()
  property("Vector builder stateful testing") = new SeqBuilderStateProperties(Vector.newBuilder[A]).property()
  property("WrappedString builder stateful testing") = new SeqBuilderStateProperties(WrappedString.newBuilder).property()

}


/** Generic stateful property testing for Seq builders
  *
  * Usage: {{{
  *   class MyCollectionProperties extends Properties("my.Collection") {
  *      property("MyCollection builder stateful testing") =
  *        new SeqBuilderStateProperties(MySeq.newBuilder[A]).property() &&
  *   }
  * }}}
  * @param arbA  gen for the elements of the Seq
  * @tparam To the type of Seq under test
  */
class SeqBuilderStateProperties[A: Arbitrary, To <: Seq[A]](newBuilder: => mutable.Builder[A, To])(implicit arbA: Arbitrary[A]) extends Commands {

  override type State = Seq[A]
  override type Sut = mutable.Builder[A, To]

  import Gen._
  val commandGen = Gen.oneOf(
    const(Clear),
    const(Result),
    choose(0, 10000).map(SizeHint(_)),
    arbA.arbitrary.map(a => AddOne(a)),
    listOf(arbA.arbitrary).map(a => AddAll(a))
  )

  override def genInitialState: Gen[State] = newBuilder.result()

  override def canCreateNewSut(newState: State, initSuts: scala.Iterable[State], runningSuts: scala.Iterable[Sut]) = true

  override def newSut(state: State): mutable.Builder[A, To] = newBuilder.addAll(state)

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State) = state.isEmpty

  override def genCommand(state: State): Gen[Command] = commandGen

  case object Clear extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.clear()
    override def nextState(state: State) = Vector.empty
    override def preCondition(state: State) = true
  }
  case object Result extends Command {
    override type Result = State
    override def postCondition(state: State, result: Try[Result]) = result.map(_.toVector) == Success(state.toVector)
    override def run(sut: Sut) = sut.result().toVector
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
    override def nextState(state: State) = state.appended(elem)
    override def preCondition(state: State) = true
  }
  case class AddAll(elems: scala.collection.immutable.Seq[A]) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.addAll(elems)
    override def nextState(state: State) = state.appendedAll(elems)
    override def preCondition(state: State) = true
  }
}
