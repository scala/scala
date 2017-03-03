package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.{mutable => cm, immutable => ci}
import scala.collection.JavaConverters._

/* Tests various maps by making sure they all agree on the same answers. */
@RunWith(classOf[JUnit4])
class SetMapConsistencyTest {
  
  trait MapBox[A] {
    protected def oor(s: String, n: Int) = throw new IllegalArgumentException(s"Out of range for $s: $n")
    def title: String
    def adders: Int
    def add(n: Int, a: A, v: Int): Unit
    def subbers: Int
    def sub(n: Int, a: A): Unit
    def getters: Int
    def get(n: Int, a: A): Int
    def fiddlers: Int
    def fiddle(n: Int): Unit
    def keys: Iterator[A]
    def has(a: A): Boolean
  }
  
  
  // Mutable map wrappers
  
  class BoxMutableMap[A, M <: cm.Map[A, Int]](m0: M, title0: String) extends MapBox[A] {
    var m = m0
    def title = title0
    def adders = 5
    def add(n: Int, a: A, v: Int) { n match {
      case 0 => m += ((a, v))
      case 1 => m(a) = v
      case 2 => m.put(a, v)
      case 3 => m = (m + ((a, v))).asInstanceOf[M]
      case 4 => m = (m ++ List((a, v))).asInstanceOf[M]
      case _ => oor("add", n)
    }}
    def subbers: Int = 3
    def sub(n: Int, a: A) { n match {
      case 0 => m -= a
      case 1 => m = (m - a).asInstanceOf[M]
      case 2 => m = m.filter(_._1 != a).asInstanceOf[M]
      case _ => oor("sub", n)
    }}
    def getters: Int = 3
    def get(n: Int, a: A) = n match {
      case 0 => m.get(a).getOrElse(-1)
      case 1 => if (m contains a) m(a) else -1
      case 2 => m.getOrElse(a, -1)
      case _ => oor("get", n)
    }
    def fiddlers: Int = 0
    def fiddle(n: Int) { oor("fiddle", n) }
    def keys = m.keysIterator
    def has(a: A) = m contains a
    override def toString = m.toString
  }
  
  def boxMlm[A] = new BoxMutableMap[A, cm.ListMap[A, Int]](new cm.ListMap[A, Int], "mutable.ListMap")
  
  def boxMhm[A] = new BoxMutableMap[A, cm.HashMap[A, Int]](new cm.HashMap[A, Int], "mutable.HashMap")
  
  def boxMohm[A] = new BoxMutableMap[A, cm.OpenHashMap[A, Int]](new cm.OpenHashMap[A, Int], "mutable.OpenHashMap")

  def boxMtm[A: Ordering] = new BoxMutableMap[A, cm.TreeMap[A, Int]](new cm.TreeMap[A, Int], "mutable.TreeMap")
  
  def boxMarm[A <: AnyRef] = new BoxMutableMap[A, cm.AnyRefMap[A, Int]](new cm.AnyRefMap[A, Int](_ => -1), "mutable.AnyRefMap") {
    private def arm: cm.AnyRefMap[A, Int] = m.asInstanceOf[cm.AnyRefMap[A, Int]]
    override def adders = 3
    override def subbers = 1
    override def getters: Int = 4
    override def get(n: Int, a: A) = n match {
      case 0 => m.get(a).getOrElse(-1)
      case 1 => m(a)
      case 2 => m.getOrElse(a, -1)
      case 3 => val x = arm.getOrNull(a); if (x==0 && !(arm contains a)) -1 else x
      case _ => oor("get", n)
    }
    override def fiddlers = 2
    override def fiddle(n: Int) { n match {
      case 0 => m = arm.clone
      case 1 => arm.repack
      case _ => oor("fiddle", n)
    }}
  }
  
  def boxMjm = new BoxMutableMap[Long, cm.LongMap[Int]](new cm.LongMap[Int](_ => -1), "mutable.LongMap") {
    private def lm: cm.LongMap[Int] = m.asInstanceOf[cm.LongMap[Int]]
    override def adders = 3
    override def subbers = 1
    override def getters: Int = 4
    override def get(n: Int, a: Long) = n match {
      case 0 => m.get(a).getOrElse(-1)
      case 1 => m(a)
      case 2 => m.getOrElse(a, -1)
      case 3 => val x = lm.getOrNull(a); if (x==0 && !(lm contains a)) -1 else x
      case _ => oor("get", n)
    }
    override def fiddlers = 2
    override def fiddle(n: Int) { n match {
      case 0 => m = lm.clone
      case 1 => lm.repack
      case _ => oor("fiddle", n)
    }}
  }
  
  def boxJavaM[A] = new BoxMutableMap[A, cm.Map[A, Int]]((new java.util.HashMap[A, Int]).asScala, "java.util.HashMap") {
    override def adders = 3
    override def subbers = 1
  }
  
  
  // Immutable map wrappers
  
  class BoxImmutableMap[A, M <: ci.Map[A, Int]](m0: M, title0: String) extends MapBox[A] {
    var m = m0
    def title = title0
    def adders = 2
    def add(n: Int, a: A, v: Int) { n match {
      case 0 => m = (m + ((a, v))).asInstanceOf[M]
      case 1 => m = (m ++ List((a, v))).asInstanceOf[M]
      case _ => oor("add", n)
    }}
    def subbers: Int = 2
    def sub(n: Int, a: A) { n match {
      case 0 => m = (m - a).asInstanceOf[M]
      case 1 => m = m.filter(_._1 != a).asInstanceOf[M]
      case _ => oor("sub", n)
    }}
    def getters: Int = 3
    def get(n: Int, a: A) = n match {
      case 0 => m.get(a).getOrElse(-1)
      case 1 => if (m contains a) m(a) else -1
      case 2 => m.getOrElse(a, -1)
      case _ => oor("get", n)
    }
    def fiddlers: Int = 0
    def fiddle(n: Int) { oor("fiddle", n) }
    def keys = m.keysIterator
    def has(a: A) = m contains a
    override def toString = m.toString
  }
  
  def boxIhm[A] = new BoxImmutableMap[A, ci.HashMap[A,Int]](new ci.HashMap[A, Int], "immutable.HashMap")
  
  def boxIim = new BoxImmutableMap[Int, ci.IntMap[Int]](ci.IntMap.empty[Int], "immutable.IntMap")
  
  def boxIjm = new BoxImmutableMap[Long, ci.LongMap[Int]](ci.LongMap.empty[Int], "immutable.LongMap")
  
  def boxIlm[A] = new BoxImmutableMap[A, ci.ListMap[A, Int]](new ci.ListMap[A, Int], "immutable.ListMap")
  
  def boxItm[A: Ordering] = new BoxImmutableMap[A, ci.TreeMap[A, Int]](new ci.TreeMap[A, Int], "immutable.TreeMap")
    
  
  // Mutable set wrappers placed into the same framework (everything returns 0)
  
  class BoxMutableSet[A, M <: cm.Set[A]](s0: M, title0: String) extends MapBox[A] {
    protected var m = s0
    def title = title0
    def adders = 5
    def add(n: Int, a: A, v: Int) { n match {
      case 0 => m += a
      case 1 => m(a) = true
      case 2 => m add a
      case 3 => m = (m + a).asInstanceOf[M]
      case 4 => m = (m ++ List(a)).asInstanceOf[M]
      case _ => oor("add", n)
    }}
    def subbers: Int = 3
    def sub(n: Int, a: A) { n match {
      case 0 => m -= a
      case 1 => m = (m - a).asInstanceOf[M]
      case 2 => m = m.filter(_ != a).asInstanceOf[M]
      case _ => oor("sub", n)
    }}
    def getters: Int = 1
    def get(n: Int, a: A) = if (m(a)) 0 else -1
    def fiddlers: Int = 0
    def fiddle(n: Int) { oor("fiddle", n) }
    def keys = m.iterator
    def has(a: A) = m(a)
    override def toString = m.toString
  }
  
  def boxMbs = new BoxMutableSet[Int, cm.BitSet](new cm.BitSet, "mutable.BitSet")
  
  def boxMhs[A] = new BoxMutableSet[A, cm.HashSet[A]](new cm.HashSet[A], "mutable.HashSet")

  def boxMts[A: Ordering] = new BoxMutableSet[A, cm.TreeSet[A]](new cm.TreeSet[A], "mutable.TreeSet")

  def boxJavaS[A] = new BoxMutableSet[A, cm.Set[A]]((new java.util.HashSet[A]).asScala, "java.util.HashSet") {
    override def adders = 3
    override def subbers = 1
  }
  
  
  // Immutable set wrappers placed into the same framework (everything returns 0)
  
  class BoxImmutableSet[A, M <: ci.Set[A]](s0: M, title0: String) extends MapBox[A] {
    protected var m = s0
    def title = title0
    def adders = 2
    def add(n: Int, a: A, v: Int) { n match {
      case 0 => m = (m + a).asInstanceOf[M]
      case 1 => m = (m ++ List(a)).asInstanceOf[M]
      case _ => oor("add", n)
    }}
    def subbers: Int = 2
    def sub(n: Int, a: A) { n match {
      case 0 => m = (m - a).asInstanceOf[M]
      case 1 => m = m.filter(_ != a).asInstanceOf[M]
      case _ => oor("sub", n)
    }}
    def getters: Int = 1
    def get(n: Int, a: A) = if (m(a)) 0 else -1
    def fiddlers: Int = 0
    def fiddle(n: Int) { oor("fiddle", n) }
    def keys = m.iterator
    def has(a: A) = m(a)
    override def toString = m.toString
  }
  
  def boxIbs = new BoxImmutableSet[Int, ci.BitSet](ci.BitSet.empty, "immutable.BitSet")
  
  def boxIhs[A] = new BoxImmutableSet[A, ci.HashSet[A]](ci.HashSet.empty[A], "mutable.HashSet")
  
  def boxIls[A] = new BoxImmutableSet[A, ci.ListSet[A]](ci.ListSet.empty[A], "mutable.ListSet")
  
  def boxIts[A: Ordering] = new BoxImmutableSet[A, ci.TreeSet[A]](ci.TreeSet.empty[A], "mutable.TreeSet")
  
  
  // Random operations on maps
  def churn[A](map1: MapBox[A], map2: MapBox[A], keys: Array[A], n: Int = 1000, seed: Int = 42, valuer: Int => Int = identity) = {
    def check = map1.keys.forall(map2 has _) && map2.keys.forall(map1 has _)
    val rn = new scala.util.Random(seed)
    var what = new StringBuilder
    what ++= "creation"
    for (i <- 0 until n) {
      if (!check) {
        val temp = map2 match {
          case b: BoxImmutableMap[_, _] => b.m match {
            case hx: ci.HashMap.HashTrieMap[_,_] =>
              val h = hx.asInstanceOf[ci.HashMap.HashTrieMap[A, Int]]
              Some((h.bitmap.toHexString, h.elems.mkString, h.size))
            case _ => None
          }
          case _ => None
        }
        throw new Exception(s"Disagreement after ${what.result} between ${map1.title} and ${map2.title} because ${map1.keys.map(map2 has _).mkString(",")} ${map2.keys.map(map1 has _).mkString(",")} at step $i:\n$map1\n$map2\n$temp")
      }
      what ++= " (%d) ".format(i)
      if (rn.nextInt(10)==0) {
        
        if (map1.fiddlers > 0) map1.fiddle({
          val n = rn.nextInt(map1.fiddlers)
          what ++= ("f"+n)
          n
        })
        if (map2.fiddlers > 0) map2.fiddle({
          val n = rn.nextInt(map2.fiddlers)
          what ++= ("F"+n)
          n
        })
      }
      if (rn.nextBoolean) {
        val idx = rn.nextInt(keys.length)
        val key = keys(rn.nextInt(keys.length))
        val n1 = rn.nextInt(map1.adders)
        val n2 = rn.nextInt(map2.adders)
        what ++= "+%s(%d,%d)".format(key,n1,n2)
        map1.add(n1, key, valuer(idx))
        map2.add(n2, key, valuer(idx))
      }
      else {
        val n = rn.nextInt(keys.length)
        val key = keys(n)
        val n1 = rn.nextInt(map1.subbers)
        val n2 = rn.nextInt(map2.subbers)
        what ++= "-%s(%d,%d)".format(key, n1, n2)
        //println(s"- $key")
        map1.sub(n1, key)
        map2.sub(n2, key)
      }
      val j = rn.nextInt(keys.length)
      val gn1 = rn.nextInt(map1.getters)
      val gn2 = rn.nextInt(map2.getters)
      val g1 = map1.get(gn1, keys(j))
      val g2 = map2.get(gn2, keys(j))
      if (g1 != g2) {
        val temp = map2 match {
          case b: BoxImmutableMap[_, _] => b.m match {
            case hx: ci.HashMap.HashTrieMap[_,_] =>
              val h = hx.asInstanceOf[ci.HashMap.HashTrieMap[A, Int]]
              val y = (ci.HashMap.empty[A, Int] ++ h).asInstanceOf[ci.HashMap.HashTrieMap[A, Int]]
              Some(((h.bitmap.toHexString, h.elems.mkString, h.size),(y.bitmap.toHexString, y.elems.mkString, y.size)))
            case _ => None
          }
          case _ => None
        }
        throw new Exception(s"Disagreement after ${what.result} between ${map1.title} and ${map2.title} on get of ${keys(j)} (#$j) on step $i: $g1 != $g2 using methods $gn1 and $gn2 resp.; in full\n$map1\n$map2\n$temp")
      }
    }
    true
  }
  
  
  // Actual tests
  val smallKeys = Array(0, 1, 42, 9127)
  val intKeys = smallKeys ++ Array(-1, Int.MaxValue, Int.MinValue, -129385)
  val longKeys = intKeys.map(_.toLong) ++ Array(Long.MaxValue, Long.MinValue, 1397198789151L, -41402148014L)
  val stringKeys = intKeys.map(_.toString) ++ Array("", null)
  val anyKeys = stringKeys.filter(_ != null) ++ Array(0L) ++ Array(true) ++ Array(math.Pi)

  @Test
  def churnIntMaps() {
    val maps = Array[() => MapBox[Int]](
      () => boxMlm[Int], () => boxMhm[Int], () => boxMohm[Int], () => boxMtm[Int], () => boxJavaM[Int],
      () => boxIim, () => boxIhm[Int], () => boxIlm[Int], () => boxItm[Int]
    )
    assert( maps.sliding(2).forall{ ms => churn(ms(0)(), ms(1)(), intKeys, 2000) } )
  }
  
  @Test
  def churnLongMaps() {
    val maps = Array[() => MapBox[Long]](
      () => boxMjm, () => boxIjm, () => boxJavaM[Long],
      () => boxMlm[Long], () => boxMhm[Long], () => boxMtm[Long], () => boxMohm[Long], () => boxIhm[Long], () => boxIlm[Long]
    )
    assert( maps.sliding(2).forall{ ms => churn(ms(0)(), ms(1)(), longKeys, 10000) } )
  }
  
  @Test
  def churnStringMaps() {
    // Note: OpenHashMap and TreeMap won't store null, so skip strings
    val maps = Array[() => MapBox[String]](
      () => boxMlm[String], () => boxMhm[String], () => boxMarm[String], () => boxJavaM[String],
      () => boxIhm[String], () => boxIlm[String]
    )
    assert( maps.sliding(2).forall{ ms => churn(ms(0)(), ms(1)(), stringKeys, 5000) } )
  }
  
  @Test
  def churnAnyMaps() {
    val maps = Array[() => MapBox[Any]](
      () => boxMlm[Any], () => boxMhm[Any], () => boxMohm[Any], () => boxJavaM[Any], () => boxIhm[Any], () => boxIlm[Any]
    )
    assert( maps.sliding(2).forall{ ms => churn(ms(0)(), ms(1)(), anyKeys, 10000) } )
  }
  
  @Test
  def churnIntSets() {
    val sets = Array[() => MapBox[Int]](
      () => boxMhm[Int], () => boxIhm[Int], () => boxJavaS[Int],
      () => boxMbs, () => boxMhs[Int], () => boxMts[Int], () => boxIbs, () => boxIhs[Int], () => boxIls[Int], () => boxIts[Int]
    )
    assert( sets.sliding(2).forall{ ms => churn(ms(0)(), ms(1)(), smallKeys, 1000, valuer = _ => 0) } )
  }
  
  @Test 
  def churnAnySets() {
    val sets = Array[() => MapBox[Any]](
      () => boxMhm[Any], () => boxIhm[Any], () => boxJavaS[Any],
      () => boxMhs[Any], () => boxIhs[Any], () => boxIls[Any]
    )
    assert( sets.sliding(2).forall{ ms => churn(ms(0)(), ms(1)(), anyKeys, 10000, valuer = _ => 0) } )
  }
  
  @Test
  def extraMutableLongMapTests() {
    import cm.{LongMap, HashMap}
    var lm = LongMap.empty[Long]
    longKeys.zipWithIndex.foreach{ case (k,i) => lm(k) = i }
    assert{ lm.map{ case (k,v) => -k*k -> v.toString }.getClass == lm.getClass }
      
    assert {
      val lm2 = new LongMap[Unit](2000000)
      for (i <- 0 until 1000000) lm2(i) = ()
      
      lm2.size == 1000000 && 
        (0 to 1100000 by 100000).forall(i => (lm2 contains i) == i < 1000000)
    }
    
    lm = LongMap(8L -> 22L, -5L -> 5L, Long.MinValue -> 0L)
    
    assert{ var s = 0L; lm.foreachKey(s += _); s == Long.MinValue + 3 }
    assert{ var s = 0L; lm.foreachValue(s += _); s == 27L }
    assert { 
      val m2 = lm.mapValuesNow(_+2)
      lm.transformValues(_+2)
      m2 == lm && !(m2 eq lm) && (for ((_,v) <- lm) yield v).sum == 33L
    }

    assert {
      val lm2 = new LongMap[String](_.toString)
      lm2 += (5L -> "fish", 0L -> "unicorn")
      val hm2 = (new HashMap[Long,String]) ++= lm2
      List(Long.MinValue, 0L, 1L, 5L).forall(i =>
        lm2.get(i) == hm2.get(i) &&
        lm2.getOrElse(i, "") == hm2.getOrElse(i, "") &&
        lm2(i) == hm2.get(i).getOrElse(i.toString) &&
        lm2.getOrNull(i) == hm2.get(i).orNull
      )
    }
  }
  
  @Test
  def extraMutableAnyRefMapTests() {
    import cm.{AnyRefMap, HashMap}
    var arm = AnyRefMap.empty[String, Int]
    stringKeys.zipWithIndex.foreach{ case (k,i) => arm(k) = i }
    
    assert{ arm.map{ case (k,v) => (if (k==null) "" else k+k) -> v.toString }.getClass == arm.getClass }
    
    assert {
      val arm2 = new AnyRefMap[java.lang.Integer,Unit](2000000)
      for (i <- 0 until 1000000) arm2(java.lang.Integer.valueOf(i)) = ()      
      arm2.size == 1000000 && 
        (0 to 1100000 by 100000).map(java.lang.Integer.valueOf).forall(i => (arm2 contains i) == i < 1000000)
    }
    
    arm = AnyRefMap("heron" -> 22, "dove" -> 5, "budgie" -> 0)
    
    assert{
      var s = ""
      arm.foreachKey(s += _)
      s.length == "herondovebudgie".length &&
      s.contains("heron") &&
      s.contains("dove") &&
      s.contains("budgie")
    }

    assert{ var s = 0L; arm.foreachValue(s += _); s == 27L }

    assert { 
      val m2 = arm.mapValuesNow(_+2)
      arm.transformValues(_+2)
      m2 == arm && !(m2 eq arm) && (for ((_,v) <- arm) yield v).sum == 33L
    }

    assert {
      val arm2 = new AnyRefMap[String, String](x => if (x==null) "null" else x)
      arm2 += ("cod" -> "fish", "Rarity" -> "unicorn")
      val hm2 = (new HashMap[String,String]) ++= arm2
      List(null, "cod", "sparrow", "Rarity").forall(i =>
        arm2.get(i) == hm2.get(i) &&
        arm2.getOrElse(i, "") == hm2.getOrElse(i, "") &&
        arm2(i) == hm2.get(i).getOrElse(if (i==null) "null" else i.toString) &&
        arm2.getOrNull(i) == hm2.get(i).orNull
      )
    }
  }
  
  @Test
  def extraFilterTests() {
    type M = scala.collection.Map[Int, Boolean]
    val manyKVs = (0 to 1000).map(i => i*i*i).map(x => x -> ((x*x*x) < 0))
    val rn = new scala.util.Random(42)
    def mhm: M = { val m = new cm.HashMap[Int, Boolean]; m ++= manyKVs; m }
    def mohm: M = { val m = new cm.OpenHashMap[Int, Boolean]; m ++= manyKVs; m }
    def ihm: M = ci.HashMap.empty[Int, Boolean] ++ manyKVs
    val densities = List(0, 0.05, 0.2, 0.5, 0.8, 0.95, 1)
    def repeat = rn.nextInt(100) < 33
    def pick(m: M, density: Double) = m.keys.filter(_ => rn.nextDouble < density).toSet
    def test: Boolean = {
      for (i <- 0 to 100) {
        var ms = List(mhm, mohm, ihm)
        do {
          val density = densities(rn.nextInt(densities.length))
          val keep = pick(ms.head, density)
          ms = ms.map(_.filter(keep contains _._1))
          if (!ms.sliding(2).forall(s => s(0) == s(1))) return false
        } while (repeat)
      }
      true
    }
    assert(test)
  }
  
  @Test
  def testSI8213() {
    val am = new scala.collection.mutable.AnyRefMap[String, Int]
    for (i <- 0 until 1024) am += i.toString -> i
    am.getOrElseUpdate("1024", { am.clear; -1 })
    assert(am == scala.collection.mutable.AnyRefMap("1024" -> -1))
    val lm = new scala.collection.mutable.LongMap[Int]
    for (i <- 0 until 1024) lm += i.toLong -> i
    lm.getOrElseUpdate(1024, { lm.clear; -1 })
    assert(lm == scala.collection.mutable.LongMap(1024L -> -1))
  }
  
  // Mutating when an iterator is in the wild shouldn't produce random junk in the iterator
  // Todo: test all sets/maps this way
  @Test
  def testSI8154() {
    def f() = {
      val xs = scala.collection.mutable.AnyRefMap[String, Int]("a" -> 1)
      val it = xs.iterator
      it.hasNext
      xs.clear()
    
      if (it.hasNext) Some(it.next)
      else None
    }
    assert(f() match {
      case Some((a,b)) if (a==null || b==null) => false
      case _ => true
    })
  }
  
  @Test
  def testSI8264() {
    val hs = Set(-2147483648, 1, -45023380, -1, 1971207058, -54312241, -234243394) - -1
    assert( hs.toList.toSet == hs )
    assert( hs == hs.toList.toSet )
  }

  @Test
  def testSI8815() {
    val lm = new scala.collection.mutable.LongMap[String]
    lm += (Long.MinValue, "min")
    lm += (-1, "neg-one")
    lm += (0, "zero")
    lm += (Long.MaxValue, "max")
    var nit = 0
    lm.iterator.foreach(_ => nit += 1)
    var nfe = 0
    lm.foreach(_ => nfe += 1)
    assert(nit == 4)
    assert(nfe == 4)
  }

  @Test
  def test_SI8727() {
    import scala.tools.testing.AssertUtil._
    type NSEE = NoSuchElementException
    val map = Map(0 -> "zero", 1 -> "one")
    val m = map.filterKeys(i => if (map contains i) true else throw new NSEE)
    assert{ (m contains 0) && (m get 0).nonEmpty }
    assertThrows[NSEE]{ m contains 2 }
    assertThrows[NSEE]{ m get 2 }
  }
}
