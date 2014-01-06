object Test extends App {

  import scala.collection.mutable.HashMap;
  import scala.collection.mutable.AnyRefMap;

  val keys = Array(
    null, "perch", "herring", "salmon", "pike", "cod", ""
  )

  val rn = new scala.util.Random(42L)
  var arm = AnyRefMap.empty[String, Int]
  val hm = HashMap.empty[String, Int]
  
  def checkConsistent = hm.forall{ case (k,v) => arm.get(k).exists(_ == v) }

  assert {
    (0 to 10000).forall{ i =>
      val k = keys(rn.nextInt(keys.length))
      if (rn.nextInt(100) < 2) arm = arm.clone()
      if (rn.nextInt(100) < 5) arm.repack()
      if (rn.nextBoolean) {
        hm += ((k, i))
        rn.nextInt(6) match {
          case 0 => arm += ((k, i))
          case 1 => arm += (k, i)
          case 2 => arm(k) = i
          case 3 => arm.put(k,i)
          case 4 => arm ++= List((k,i))
          case _ => if (!arm.contains(k)) arm.getOrElseUpdate(k,i) 
                    else arm += (k,i)
        }
      }
      else {
        hm -= k
        rn.nextInt(2) match {
          case 0 => arm -= k
          case _ => arm --= List(k)
        }
      }
      checkConsistent
    }
  }
  
  assert {
    val mapped = 
      arm.map{ case (k,v) => (if (k==null) "" else k+k) -> v.toString }
    mapped.getClass == arm.getClass
  }
  
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

