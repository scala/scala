
import scala.language.reflectiveCalls

class Slazz {
  val s1 = Symbol("myFirstSymbol")
  val s2 = Symbol("mySecondSymbol")
  def s3 = Symbol("myThirdSymbol")
  var s4: Symbol = null

  s4 = Symbol("myFourthSymbol")
}

class Base {
  val basesymbol = Symbol("symbase")
}

class Sub extends Base {
  val subsymbol = Symbol("symsub")
}

trait Signs {
  val ind = Symbol("indication")
  val trace = Symbol("trace")
}

trait Lazy1 {
  lazy val v1 = "lazy v1"
  lazy val s1 = Symbol("lazySymbol1")
}

trait Lazy2 {
  lazy val v2 = "lazy v2"
  lazy val s2 = Symbol("lazySymbol2")
}

trait Lazy3 {
  lazy val v3 = "lazy v3"
  lazy val s3 = Symbol("lazySymbol3")
}

object SingletonOfLazyness {
  lazy val lazysym = Symbol("lazySymbol")
  lazy val another = Symbol("another")
  lazy val lastone = Symbol("lastone")
}

/*
 * Tests symbols to see if they work correct.
 */
object Test {
  class Inner {
    val simba = Symbol("smba")
    var mfs: Symbol = null
    mfs = Symbol("mfsa")
  }

  object InnerObject {
    val o1 = Symbol("aaa")
    val o2 = Symbol("ddd")
  }

  def aSymbol = Symbol("myFirstSymbol")
  val anotherSymbol = Symbol("mySecondSymbol")

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
    assert(scl.s3 == Symbol("myThirdSymbol"))
    assert(scl.s4 == Symbol.apply("myFourthSymbol"))
    assert(scl.s1 == Symbol("myFirstSymbol"))
  }

  def testForLoop: Unit = {
    for (i <- 0 until 100) List("Val" + i)
  }

  def testInnerClasses: Unit = {
    val innerPower = new Inner
    assert(innerPower.simba == Symbol("smba"))
    assert(innerPower.mfs == Symbol("mfsa"))
  }

  def testInnerObjects: Unit = {
    assert(InnerObject.o1 == Symbol("aaa"))
    assert(InnerObject.o2 == Symbol("ddd"))
  }

  def testWithHashMaps: Unit = {
    val map = new collection.mutable.HashMap[Symbol, Symbol]
    map.put(InnerObject.o1, Symbol("smba"))
    map.put(InnerObject.o2, Symbol("mfsa"))
    map.put(Symbol("WeirdKey" + 1), Symbol("Weird" + "Val" + 1))
    assert(map(Symbol("aaa")) == Symbol("smba"))
    assert(map(Symbol("ddd")) == Symbol("mfsa"))
    assert(map(Symbol("WeirdKey1")) == Symbol("WeirdVal1"))

    map.clear
    for (i <- 0 until 100) map.put(Symbol("symKey" + i), Symbol("symVal" + i))
    assert(map(Symbol("symKey15")) == Symbol("symVal15"))
    assert(map(Symbol("symKey22")) == Symbol("symVal22"))
    assert(map(Symbol("symKey73")) == Symbol("symVal73"))
    assert(map(Symbol("symKey56")) == Symbol("symVal56"))
    assert(map(Symbol("symKey91")) == Symbol("symVal91"))
  }

  def testLists: Unit = {
    var lst: List[Symbol] = Nil
    for (i <- 0 until 100) lst ::= Symbol("lsym" + (99 - i))
    assert(lst(0) == Symbol("lsym0"))
    assert(lst(10) == Symbol("lsym10"))
    assert(lst(30) == Symbol("lsym30"))
    assert(lst(40) == Symbol("lsym40"))
    assert(lst(65) == Symbol("lsym65"))
    assert(lst(90) == Symbol("lsym90"))
  }

  def testAnonymous: Unit = { // TODO complaints classdef can't be found for some reason, runs fine in my case
    // val anon = () => {
    //   val simba = Symbol("smba")
    //   simba
    // }
    // val an2 = () => {
    //   object nested {
    //     val m = Symbol("mfsa")
    //   }
    //   nested.m
    // }
    // val an3 = () => {
    //   object nested {
    //     val f = () => {
    //       Symbol("layered")
    //     }
    //     def gets = f()
    //   }
    //   nested.gets
    // }
    // val inner = new Inner
    // assert(anon() == inner.simba)
    // assert(anon().toString == "'smba")
    // assert(an2() == Symbol("mfsa"))
    // assert(an3() == Symbol("layered" + ""))
  }

  def testNestedObject: Unit = {
    object nested {
      def sign = Symbol("sign")
      def insignia = Symbol("insignia")
    }
    assert(nested.sign == Symbol("sign"))
    assert(nested.insignia == Symbol("insignia"))
    assert((Symbol("insignia")).toString == "'insignia")
  }

  def testInheritance: Unit = {
    val base = new Base
    val sub = new Sub
    assert(base.basesymbol == Symbol("symbase"))
    assert(sub.subsymbol == Symbol("symsub"))
    assert(sub.basesymbol == Symbol("symbase"))

    val anon = new Sub {
      def subsubsymbol = Symbol("symsubsub")
    }
    assert(anon.subsubsymbol == Symbol("symsubsub"))
    assert(anon.subsymbol == Symbol("symsub"))
    assert(anon.basesymbol == Symbol("symbase"))

    object nested extends Sub {
      def objsymbol = Symbol("symobj")
    }
    assert(nested.objsymbol == Symbol("symobj"))
    assert(nested.subsymbol == Symbol("symsub"))
    assert(nested.basesymbol == Symbol("symbase"))
    assert((Symbol("symbase")).toString == "'symbase")
  }

  def testTraits: Unit = {
    val fromTrait = new AnyRef with Signs {
      def traitsymbol = Symbol("traitSymbol")
    }

    assert(fromTrait.traitsymbol == Symbol("traitSymbol"))
    assert(fromTrait.ind == Symbol("indication"))
    assert(fromTrait.trace == Symbol("trace"))
    assert((Symbol("trace")).toString == "'trace")

    trait Compl {
      val s1 = Symbol("s1")
      def s2 = Symbol("s2")
      object inner {
        val s3 = Symbol("s3")
        val s4 = Symbol("s4")
      }
    }

    val compl = new Sub with Signs with Compl
    assert(compl.s1 == Symbol("s1"))
    assert(compl.s2 == Symbol("s2"))
    assert(compl.inner.s3 == Symbol("s3"))
    assert(compl.inner.s4 == Symbol("s4"))
    assert(compl.ind == Symbol("indication"))
    assert(compl.trace == Symbol("trace"))
    assert(compl.subsymbol == Symbol("symsub"))
    assert(compl.basesymbol == Symbol("symbase"))

    object Local extends Signs with Compl {
      val s5 = Symbol("s5")
      def s6 = Symbol("s6")
      object inner2 {
        val s7 = Symbol("s7")
        def s8 = Symbol("s8")
      }
    }
    assert(Local.s5 == Symbol("s5"))
    assert(Local.s6 == Symbol("s6"))
    assert(Local.inner2.s7 == Symbol("s7"))
    assert(Local.inner2.s8 == Symbol("s8"))
    assert(Local.inner.s3 == Symbol("s3"))
    assert(Local.inner.s4 == Symbol("s4"))
    assert(Local.s1 == Symbol("s1"))
    assert(Local.s2 == Symbol("s2"))
    assert(Local.trace == Symbol("trace"))
    assert(Local.ind == Symbol("indication"))
    assert((Symbol("s8")).toString == "'s8")
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
    assert(l3.s3 == Symbol("lazySymbol3"))
  }

  def testLazyObjects: Unit = {
    assert(SingletonOfLazyness.lazysym == Symbol("lazySymbol"))
    assert(SingletonOfLazyness.another == Symbol("ano" + "ther"))
    assert((SingletonOfLazyness.lastone).toString == "'lastone")

    object nested {
      lazy val sym1 = Symbol("snested1")
      lazy val sym2 = Symbol("snested2")
    }

    assert(nested.sym1 == Symbol("snested1"))
    assert(nested.sym2 == Symbol("snested" + "2"))
  }
}
