
import scala.language.reflectiveCalls

class Slazz {
  val s1 = sym"myFirstSymbol"
  val s2 = sym"mySecondSymbol"
  def s3 = sym"myThirdSymbol"
  var s4: Symbol = null

  s4 = sym"myFourthSymbol"
}

class Base {
  val basesymbol = sym"symbase"
}

class Sub extends Base {
  val subsymbol = sym"symsub"
}

trait Signs {
  val ind = sym"indication"
  val trace = sym"trace"
}

trait Lazy1 {
  lazy val v1 = "lazy v1"
  lazy val s1 = sym"lazySymbol1"
}

trait Lazy2 {
  lazy val v2 = "lazy v2"
  lazy val s2 = sym"lazySymbol2"
}

trait Lazy3 {
  lazy val v3 = "lazy v3"
  lazy val s3 = sym"lazySymbol3"
}

object SingletonOfLazyness {
  lazy val lazysym = sym"lazySymbol"
  lazy val another = sym"another"
  lazy val lastone = sym"lastone"
}

/*
 * Tests symbols to see if they work correct.
 */
object Test {
  class Inner {
    val simba = sym"smba"
    var mfs: Symbol = null
    mfs = Symbol("mfsa")
  }

  object InnerObject {
    val o1 = sym"aaa"
    val o2 = sym"ddd"
  }

  def aSymbol = sym"myFirstSymbol"
  val anotherSymbol = sym"mySecondSymbol"

  def main(args: Array[String]): Unit = {
    testLiterals
    testForLoop
    testInnerClasses
    testInnerObjects
    testWithHashMaps
    testLists
    testAnonymous
    testNestedObject
    testInheritance
    testTraits
    testLazyTraits
    testLazyObjects
  }

  def testLiterals: Unit = {
    val scl = new Slazz
    assert(scl.s1 == aSymbol)
    assert(scl.s2 == anotherSymbol)
    assert(scl.s3 == sym"myThirdSymbol")
    assert(scl.s4 == Symbol.apply("myFourthSymbol"))
    assert(scl.s1 == Symbol("myFirstSymbol"))
  }

  def testForLoop: Unit = {
    for (i <- 0 until 100) List("Val" + i)
  }

  def testInnerClasses: Unit = {
    val innerPower = new Inner
    assert(innerPower.simba == sym"smba")
    assert(innerPower.mfs == sym"mfsa")
  }

  def testInnerObjects: Unit = {
    assert(InnerObject.o1 == sym"aaa")
    assert(InnerObject.o2 == sym"ddd")
  }

  def testWithHashMaps: Unit = {
    val map = new collection.mutable.HashMap[Symbol, Symbol]
    map.put(InnerObject.o1, sym"smba")
    map.put(InnerObject.o2, sym"mfsa")
    map.put(Symbol("WeirdKey" + 1), Symbol("Weird" + "Val" + 1))
    assert(map(sym"aaa") == sym"smba")
    assert(map(sym"ddd") == sym"mfsa")
    assert(map(sym"WeirdKey1") == Symbol("WeirdVal1"))

    map.clear
    for (i <- 0 until 100) map.put(Symbol("symKey" + i), Symbol("symVal" + i))
    assert(map(Symbol("symKey15")) == Symbol("symVal15"))
    assert(map(sym"symKey22") == sym"symVal22")
    assert(map(sym"symKey73") == sym"symVal73")
    assert(map(sym"symKey56") == sym"symVal56")
    assert(map(sym"symKey91") == sym"symVal91")
  }

  def testLists: Unit = {
    var lst: List[Symbol] = Nil
    for (i <- 0 until 100) lst ::= Symbol("lsym" + (99 - i))
    assert(lst(0) == sym"lsym0")
    assert(lst(10) == sym"lsym10")
    assert(lst(30) == sym"lsym30")
    assert(lst(40) == sym"lsym40")
    assert(lst(65) == sym"lsym65")
    assert(lst(90) == sym"lsym90")
  }

  def testAnonymous: Unit = { // TODO complaints classdef can't be found for some reason, runs fine in my case
    // val anon = () => {
    //   val simba = sym"smba"
    //   simba
    // }
    // val an2 = () => {
    //   object nested {
    //     val m = sym"mfsa"
    //   }
    //   nested.m
    // }
    // val an3 = () => {
    //   object nested {
    //     val f = () => {
    //       sym"layered"
    //     }
    //     def gets = f()
    //   }
    //   nested.gets
    // }
    // val inner = new Inner
    // assert(anon() == inner.simba)
    // assert(anon().toString == "'smba")
    // assert(an2() == sym"mfsa")
    // assert(an3() == Symbol("layered" + ""))
  }

  def testNestedObject: Unit = {
    object nested {
      def sign = sym"sign"
      def insignia = sym"insignia"
    }
    assert(nested.sign == sym"sign")
    assert(nested.insignia == sym"insignia")
    assert((sym"insignia").toString == "'insignia")
  }

  def testInheritance: Unit = {
    val base = new Base
    val sub = new Sub
    assert(base.basesymbol == sym"symbase")
    assert(sub.subsymbol == sym"symsub")
    assert(sub.basesymbol == sym"symbase")

    val anon = new Sub {
      def subsubsymbol = sym"symsubsub"
    }
    assert(anon.subsubsymbol == sym"symsubsub")
    assert(anon.subsymbol == sym"symsub")
    assert(anon.basesymbol == sym"symbase")

    object nested extends Sub {
      def objsymbol = sym"symobj"
    }
    assert(nested.objsymbol == sym"symobj")
    assert(nested.subsymbol == sym"symsub")
    assert(nested.basesymbol == sym"symbase")
    assert((sym"symbase").toString == "'symbase")
  }

  def testTraits: Unit = {
    val fromTrait = new AnyRef with Signs {
      def traitsymbol = sym"traitSymbol"
    }

    assert(fromTrait.traitsymbol == sym"traitSymbol")
    assert(fromTrait.ind == sym"indication")
    assert(fromTrait.trace == sym"trace")
    assert((sym"trace").toString == "'trace")

    trait Compl {
      val s1 = sym"s1"
      def s2 = sym"s2"
      object inner {
        val s3 = sym"s3"
        val s4 = sym"s4"
      }
    }

    val compl = new Sub with Signs with Compl
    assert(compl.s1 == sym"s1")
    assert(compl.s2 == sym"s2")
    assert(compl.inner.s3 == sym"s3")
    assert(compl.inner.s4 == sym"s4")
    assert(compl.ind == sym"indication")
    assert(compl.trace == sym"trace")
    assert(compl.subsymbol == sym"symsub")
    assert(compl.basesymbol == sym"symbase")

    object Local extends Signs with Compl {
      val s5 = sym"s5"
      def s6 = sym"s6"
      object inner2 {
        val s7 = sym"s7"
        def s8 = sym"s8"
      }
    }
    assert(Local.s5 == sym"s5")
    assert(Local.s6 == sym"s6")
    assert(Local.inner2.s7 == sym"s7")
    assert(Local.inner2.s8 == sym"s8")
    assert(Local.inner.s3 == sym"s3")
    assert(Local.inner.s4 == sym"s4")
    assert(Local.s1 == sym"s1")
    assert(Local.s2 == sym"s2")
    assert(Local.trace == sym"trace")
    assert(Local.ind == sym"indication")
    assert((sym"s8").toString == "'s8")
  }

  def testLazyTraits: Unit = {
    val l1 = new AnyRef with Lazy1
    val l2 = new AnyRef with Lazy2
    val l3 = new AnyRef with Lazy3

    l1.v1
    l2.v2
    l3.v3
    assert((l1.s1).toString == "'lazySymbol1")
    assert(l2.s2 == Symbol("lazySymbol" + 2))
    assert(l3.s3 == sym"lazySymbol3")
  }

  def testLazyObjects: Unit = {
    assert(SingletonOfLazyness.lazysym == sym"lazySymbol")
    assert(SingletonOfLazyness.another == Symbol("ano" + "ther"))
    assert((SingletonOfLazyness.lastone).toString == "'lastone")

    object nested {
      lazy val sym1 = sym"snested1"
      lazy val sym2 = sym"snested2"
    }

    assert(nested.sym1 == sym"snested1")
    assert(nested.sym2 == Symbol("snested" + "2"))
  }

}












