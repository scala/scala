



class Slazz {
  val s1 = 'myFirstSymbol
  val s2 = 'mySecondSymbol
  def s3 = 'myThirdSymbol
  var s4: Symbol = null

  s4 = 'myFourthSymbol
}

class Base {
  val basesymbol = 'symbase
}

class Sub extends Base {
  val subsymbol = 'symsub
}

trait Signs {
  val ind = 'indication
  val trace = 'trace
}

trait Lazy1 {
  lazy val v1 = "lazy v1"
  lazy val s1 = 'lazySymbol1
}

trait Lazy2 {
  lazy val v2 = "lazy v2"
  lazy val s2 = 'lazySymbol2
}

trait Lazy3 {
  lazy val v3 = "lazy v3"
  lazy val s3 = 'lazySymbol3
}

object SingletonOfLazyness {
  lazy val lazysym = 'lazySymbol
  lazy val another = 'another
  lazy val lastone = 'lastone
}

/*
 * Tests symbols to see if they work correct.
 */
object Test {
  class Inner {
    val simba = 'smba
    var mfs: Symbol = null
    mfs = Symbol("mfsa")
  }

  object InnerObject {
    val o1 = 'aaa
    val o2 = 'ddd
  }

  def aSymbol = 'myFirstSymbol
  val anotherSymbol = 'mySecondSymbol

  def main(args: Array[String]) {
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

  def testLiterals {
    val scl = new Slazz
    assert(scl.s1 == aSymbol)
    assert(scl.s2 == anotherSymbol)
    assert(scl.s3 == 'myThirdSymbol)
    assert(scl.s4 == Symbol.apply("myFourthSymbol"))
    assert(scl.s1 == Symbol("myFirstSymbol"))
  }

  def testForLoop {
    for (i <- 0 until 100) List("Val" + i)
  }

  def testInnerClasses {
    val innerPower = new Inner
    assert(innerPower.simba == 'smba)
    assert(innerPower.mfs == 'mfsa)
  }

  def testInnerObjects {
    assert(InnerObject.o1 == 'aaa)
    assert(InnerObject.o2 == 'ddd)
  }

  def testWithHashMaps {
    val map = new collection.mutable.HashMap[Symbol, Symbol]
    map.put(InnerObject.o1, 'smba)
    map.put(InnerObject.o2, 'mfsa)
    map.put(Symbol("WeirdKey" + 1), Symbol("Weird" + "Val" + 1))
    assert(map('aaa) == 'smba)
    assert(map('ddd) == 'mfsa)
    assert(map('WeirdKey1) == Symbol("WeirdVal1"))

    map.clear
    for (i <- 0 until 100) map.put(Symbol("symKey" + i), Symbol("symVal" + i))
    assert(map(Symbol("symKey15")) == Symbol("symVal15"))
    assert(map('symKey22) == 'symVal22)
    assert(map('symKey73) == 'symVal73)
    assert(map('symKey56) == 'symVal56)
    assert(map('symKey91) == 'symVal91)
  }

  def testLists {
    var lst: List[Symbol] = Nil
    for (i <- 0 until 100) lst ::= Symbol("lsym" + (99 - i))
    assert(lst(0) == 'lsym0)
    assert(lst(10) == 'lsym10)
    assert(lst(30) == 'lsym30)
    assert(lst(40) == 'lsym40)
    assert(lst(65) == 'lsym65)
    assert(lst(90) == 'lsym90)
  }

  def testAnonymous { // TODO complaints classdef can't be found for some reason, runs fine in my case
    // val anon = () => {
    //   val simba = 'smba
    //   simba
    // }
    // val an2 = () => {
    //   object nested {
    // 	val m = 'mfsa
    //   }
    //   nested.m
    // }
    // val an3 = () => {
    //   object nested {
    // 	val f = () => {
    // 	  'layered
    // 	}
    // 	def gets = f()
    //   }
    //   nested.gets
    // }
    // val inner = new Inner
    // assert(anon() == inner.simba)
    // assert(anon().toString == "'smba")
    // assert(an2() == 'mfsa)
    // assert(an3() == Symbol("layered" + ""))
  }

  def testNestedObject {
    object nested {
      def sign = 'sign
      def insignia = 'insignia
    }
    assert(nested.sign == 'sign)
    assert(nested.insignia == 'insignia)
    assert(('insignia).toString == "'insignia")
  }

  def testInheritance {
    val base = new Base
    val sub = new Sub
    assert(base.basesymbol == 'symbase)
    assert(sub.subsymbol == 'symsub)
    assert(sub.basesymbol == 'symbase)

    val anon = new Sub {
      def subsubsymbol = 'symsubsub
    }
    assert(anon.subsubsymbol == 'symsubsub)
    assert(anon.subsymbol == 'symsub)
    assert(anon.basesymbol == 'symbase)

    object nested extends Sub {
      def objsymbol = 'symobj
    }
    assert(nested.objsymbol == 'symobj)
    assert(nested.subsymbol == 'symsub)
    assert(nested.basesymbol == 'symbase)
    assert(('symbase).toString == "'symbase")
  }

  def testTraits {
    val fromTrait = new AnyRef with Signs {
      def traitsymbol = 'traitSymbol
    }

    assert(fromTrait.traitsymbol == 'traitSymbol)
    assert(fromTrait.ind == 'indication)
    assert(fromTrait.trace == 'trace)
    assert(('trace).toString == "'trace")

    trait Compl {
      val s1 = 's1
      def s2 = 's2
      object inner {
	val s3 = 's3
	val s4 = 's4
      }
    }

    val compl = new Sub with Signs with Compl
    assert(compl.s1 == 's1)
    assert(compl.s2 == 's2)
    assert(compl.inner.s3 == 's3)
    assert(compl.inner.s4 == 's4)
    assert(compl.ind == 'indication)
    assert(compl.trace == 'trace)
    assert(compl.subsymbol == 'symsub)
    assert(compl.basesymbol == 'symbase)

    object Local extends Signs with Compl {
      val s5 = 's5
      def s6 = 's6
      object inner2 {
	val s7 = 's7
	def s8 = 's8
      }
    }
    assert(Local.s5 == 's5)
    assert(Local.s6 == 's6)
    assert(Local.inner2.s7 == 's7)
    assert(Local.inner2.s8 == 's8)
    assert(Local.inner.s3 == 's3)
    assert(Local.inner.s4 == 's4)
    assert(Local.s1 == 's1)
    assert(Local.s2 == 's2)
    assert(Local.trace == 'trace)
    assert(Local.ind == 'indication)
    assert(('s8).toString == "'s8")
  }

  def testLazyTraits {
    val l1 = new AnyRef with Lazy1
    val l2 = new AnyRef with Lazy2
    val l3 = new AnyRef with Lazy3

    l1.v1
    l2.v2
    l3.v3
    assert((l1.s1).toString == "'lazySymbol1")
    assert(l2.s2 == Symbol("lazySymbol" + 2))
    assert(l3.s3 == 'lazySymbol3)
  }

  def testLazyObjects {
    assert(SingletonOfLazyness.lazysym == 'lazySymbol)
    assert(SingletonOfLazyness.another == Symbol("ano" + "ther"))
    assert((SingletonOfLazyness.lastone).toString == "'lastone")

    object nested {
      lazy val sym1 = 'snested1
      lazy val sym2 = 'snested2
    }

    assert(nested.sym1 == 'snested1)
    assert(nested.sym2 == Symbol("snested" + "2"))
  }

}












