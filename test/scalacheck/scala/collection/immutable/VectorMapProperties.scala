package scala.collection.immutable

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._
import commands.Commands

import scala.util.Try


object VectorMapProperties extends Properties("immutable.VectorMap") {

  type K = Int
  type V = Int
  type T = (K, V)

  property("internal underlying index match") = forAll { m: Map[K, V] =>
    !m.isEmpty ==> {
      val vm = VectorMap.from(m)
      val last = vm.keys.last
      vm.fields(vm.underlying(last)._1) == last
    }
  }

  property("internal underlying and keys length") = forAll { m: Map[K, V] => {
      val vm = VectorMap.from(m)
      vm.underlying.size == vm.keys.length
    }
  }

  property("internal underlying and index are consistent after removal") = forAll { (m: Map[K, V]) =>
    m.size >= 3 ==> {
      val v = Vector.from(m)
      val random = v(new scala.util.Random().nextInt(v.size))
      val vm = VectorMap.from(v)
      val removed = vm - random._1
      removed.underlying.forall { case (k, (s, v)) => removed.fields(s) == k }
      removed.fields.zipWithIndex.forall {
        case (k: K, s) => removed.underlying(k)._1 == s
        case _ => true
      }
    }
  }

  property("repeatedly removing to empty is empty at end") = forAll { (m: Map[K, V]) => {
      var vm = VectorMap.from(m)
      val sz1 = vm.size
      for (k <- vm.keys) vm = vm.removed(k)
      val sz2 = vm.size
      for (n <- 0 until sz1) vm = vm + (n -> n)
      val sz3 = vm.size
      for (k <- vm.keys) vm = vm.removed(k)
      vm.size == 0 && sz2 == 0 && sz3 == sz1
    }
  }

  property("nullable types for keys and values") = forAll { m: Map[String, String] => {
      val vm = VectorMap.from(m)
      vm.underlying.size == vm.keys.length
    }
  }

  property("internal underlying init size of non empty is consistent with original size and keys init size") = forAll { m: Map[K, V] =>
    m.size >= 1 ==> {
      val vm = VectorMap.from(m)
      val sz = vm.size - 1
      vm.underlying.init.size == sz && vm.keys.init.size == sz
    }
  }

  property("vectorMap.last == vectorMap.toList.last, after some operations") = {
    //After vectorMap.updated(key, value), removed(key), tail, and init, verify if vectorMap.last == vectorMap.toList.last is satisfied.
    object VectorMapCommandsChecker extends Commands {
      //Perform the same operation on both maps and keep them in the same state.
      override type State = SeqMap[K, V]
      override type Sut = VectorMap[K, V]

      override def canCreateNewSut(newState: SeqMap[K, V], initSuts: scala.Iterable[SeqMap[K, V]], runningSuts: scala.Iterable[VectorMap[K, V]]): Boolean = true
      override def destroySut(sut: VectorMap[K, V]): Unit = ()
      override def newSut(state: SeqMap[K, V]): VectorMap[K, V] = VectorMap.from(state)
      override def initialPreCondition(state: SeqMap[K, V]): Boolean = true
      override def genInitialState: Gen[SeqMap[K, V]] = implicitly[Arbitrary[Map[K, V]]].arbitrary.map(m => SeqMap.from(m))
      override def genCommand(state: State): Gen[VectorMapCommandsChecker.Command] = {
        if (state.isEmpty) {
          append(state)
        }else {
          Gen.oneOf(removeRandom(state), updateRandom(state), append(state), tail(state), init(state))
        }
      }

      def removeRandom(vm: State): Gen[Remove] = Gen.delay{
        val random = new scala.util.Random()
        const(Remove(random.shuffle(vm.keysIterator).next()))
      }
      def updateRandom(vm: State): Gen[Update] = Gen.delay{
        val random = new scala.util.Random()
        val key = random.shuffle(vm.keysIterator).next()
        const(Update(key, key))
      }
      def append(state: State): Gen[Update] = Gen.delay {
        if (state.isEmpty) const(Update(1, 1))
        else {
          val max = state.maxBy(_._1)
          const(Update(max._1 + 1, max._1 + 1))
        }
      }
      def tail(state: State): Gen[Tail.type] = {
        const(Tail)
      }
      def init(state: State): Gen[Init.type ] = {
        const(Init)
      }

      case class Remove(key: K) extends Command {
        override type Result = Sut
        override def run(sut: VectorMap[K, V]): VectorMap[K, V] = sut.removed(key)
        override def nextState(state: State): State = state.removed(key)
        override def preCondition(state: State): Boolean = true
        override def postCondition(state: State, result: Try[VectorMap[K, V]]): Prop = result.map(_.lastOption) == result.map(_.toList.lastOption)
      }
      case class Update(key: K, value: V) extends Command {
        override type Result = Sut
        override def run(sut: VectorMap[K, V]): VectorMap[K, V] = sut.updated(key, value)
        override def nextState(state: State): State = state.updated(key, value)
        override def preCondition(state: State): Boolean = true
        override def postCondition(state: State, result: Try[VectorMap[K, V]]): Prop = result.map(_.last) == result.map(_.toList.last)
      }
      case object Tail extends Command {
        override type Result = Sut
        override def run(sut: VectorMap[K, V]): VectorMap[K, V] = sut.tail
        override def nextState(state: State): State = state.tail
        override def preCondition(state: State): Boolean = true
        override def postCondition(state: State, result: Try[VectorMap[K, V]]): Prop = result.map(_.lastOption) == result.map(_.toList.lastOption)
      }
      case object Init extends Command {
        override type Result = Sut
        override def run(sut: VectorMap[K, V]): VectorMap[K, V] = sut.init
        override def nextState(state: State): State = state.init
        override def preCondition(state: State): Boolean = true
        override def postCondition(state: State, result: Try[VectorMap[K, V]]): Prop = result.map(_.lastOption) == result.map(_.toList.lastOption)
      }
    }
    VectorMapCommandsChecker.property()
  }
}