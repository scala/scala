object Test extends App {

  import scala.collection.mutable.HashMap;
  import scala.collection.mutable.LongMap;

  val keys = Array(
    Long.MinValue, Int.MinValue - 1L, Int.MinValue, -9127, -1,
    0, 1, 9127, Int.MaxValue, Long.MaxValue
  )

  val rn = new scala.util.Random(42L)
  var lm = LongMap.empty[Long]
  val hm = HashMap.empty[Long,Long]
  
  def checkConsistent = hm.forall{ case (k,v) => lm.get(k).exists(_ == v) }

  assert {
    (0 to 10000).forall{ i =>
      val k = keys(rn.nextInt(keys.length))
      if (rn.nextInt(100) < 2) lm = lm.clone()
      if (rn.nextInt(100) < 5) lm.repack()
      if (rn.nextBoolean) {
        hm += ((k, i))
        rn.nextInt(6) match {
          case 0 => lm += ((k, i))
          case 1 => lm += (k, i)
          case 2 => lm(k) = i
          case 3 => lm.put(k,i)
          case 4 => lm ++= List((k,i))
          case _ => if (!lm.contains(k)) lm.getOrElseUpdate(k,i) 
                    else lm += (k,i)
        }
      }
      else {
        hm -= k
        rn.nextInt(2) match {
          case 0 => lm -= k
          case _ => lm --= List(k)
        }
      }
      checkConsistent
    }
  }
    
  assert {
    lm.map{ case (k,v) => -k*k -> v.toString }.getClass == lm.getClass
  }
  
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
