package scala.collection.immutable

import org.scalacheck.Properties

import scala.language.higherKinds
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.commands.Commands

import scala.collection.mutable
import scala.util.{Success, Try}

object MapProperties extends Properties("immutable.Map builder implementations"){

  type K = String
  type V = String
  type T = (K, V)

  property("ListMap builder stateful testing") = new MapBuilderStateProperties(HashMap.empty[K, V], ListMap.newBuilder[K, V]).property()
  property("SortedMap builder stateful testing") = new MapBuilderStateProperties(ListMap.empty[K, V], SortedMap.newBuilder[K, V]).property()
  property("HashMap builder stateful testing") = new MapBuilderStateProperties(ListMap.empty[K, V], HashMap.newBuilder[K, V]).property()
  property("Map builder stateful testing") = new MapBuilderStateProperties(ListMap.empty[K, V], Map.newBuilder[K, V]).property()
  property("VectorMap builder stateful testing") = new MapBuilderStateProperties(ListMap.empty[K, V], VectorMap.newBuilder[K, V]).property()
  property("IntMap builder stateful testing") = new MapBuilderStateProperties(ListMap.empty[Int, Long], IntMap.newBuilder[Long]).property()
  property("LongMap builder stateful testing") = new MapBuilderStateProperties(ListMap.empty[Long, Int], LongMap.newBuilder[Int]).property()

}

/** Generic stateful property testing for maps
  *
  * Usage: {{{
  *   class MyMapProperties extends Properties("my.Map") {
  *
  *      property("MyMapProperties builder stateful testing") =
  *        new MapBuilderStateProperties[K, V, ListMap, MyMap](ListMap, MyMap).property() &&
  *        new MapBuilderStateProperties[K, V, Map, MyMap](Map, MyMap).property() &&
  *        new MapBuilderStateProperties[K, V, VectorMap, MyMap](VectorMap, MyMap).property()
  *   }
  * }}}
  *
  * @param newEmptyControlMap Produce a new empty control map. This map is assumed to be correct.
  *                       That is, it is assumed it can immutably append/concatenate properly.
  *                       It's a good idea to cross validate against two or more control maps.
  * @param newBuilder produce a new builder of the map under test
  *
  * @param tupleGen  gen for the key-values of this map
  * @tparam K type of the Key
  * @tparam V type of the Value
  * @tparam ControlMap the type of the control map implementation
  * @tparam M the type of map under test
  */
class MapBuilderStateProperties[K, V, ControlMap <: Map[K, V], M <: Map[K, V]](
  newEmptyControlMap: => ControlMap,
  newBuilder: => mutable.Builder[(K, V), M])(implicit tupleGen: Arbitrary[(K, V)]) extends Commands {

  override type State = ControlMap
  override type Sut = mutable.Builder[(K, V), M]

  override def genInitialState: Gen[ControlMap] = newEmptyControlMap

  override def canCreateNewSut(
                                newState: State,
                                initSuts: scala.Iterable[State],
                                runningSuts: scala.Iterable[mutable.Builder[(K, V), M]]) = true

  override def newSut(state: ControlMap) = newBuilder.addAll(state)

  override def destroySut(sut: mutable.Builder[(K, V), M]): Unit = ()

  override def initialPreCondition(state: ControlMap) = state.isEmpty

  override def genCommand(state: State) = {
    import Gen._
    oneOf(
      const(Clear),
      const(Result),
      arbInt.arbitrary.map(SizeHint),
      tupleGen.arbitrary.map(AddOne),
      listOf(tupleGen.arbitrary).map(AddAll)
    )
  }

  case object Clear extends UnitCommand {
    override def postCondition(state: ControlMap, success: Boolean) = success
    override def run(sut: mutable.Builder[(K, V), M]) = sut.clear()
    override def nextState(state: ControlMap) = newEmptyControlMap
    override def preCondition(state: ControlMap) = true
  }
  case object Result extends Command {
    override type Result = M
    override def postCondition(state: ControlMap, result: Try[Result]) = result == Success(state)
    override def run(sut: mutable.Builder[(K, V), M]) = sut.result()
    override def nextState(state: ControlMap) = state
    override def preCondition(state: ControlMap) = true
  }
  case class SizeHint(size: Int) extends UnitCommand {
    override def postCondition(state: ControlMap, success: Boolean) = success
    override def run(sut: mutable.Builder[(K, V), M]) = sut.sizeHint(size)
    override def nextState(state: ControlMap) = state
    override def preCondition(state: ControlMap) = true
  }
  case class AddOne(elem: (K, V)) extends UnitCommand {
    override def postCondition(state: ControlMap, success: Boolean) = success
    override def run(sut: mutable.Builder[(K, V), M]) = sut.addOne(elem)
    override def nextState(state: ControlMap) =
      state.updated(elem._1, elem._2).asInstanceOf[ControlMap]
    override def preCondition(state: ControlMap) = true
  }
  case class AddAll(elems: Seq[(K, V)]) extends UnitCommand {
    override def postCondition(state: ControlMap, success: Boolean) = success
    override def run(sut: mutable.Builder[(K, V), M]) = sut.addAll(elems)
    override def nextState(state: ControlMap) =
      state.concat(elems).asInstanceOf[ControlMap]
    override def preCondition(state: ControlMap) = true
  }
}
