object SetBug {
  import scala.collection.immutable.{ Set => ImmutSet }
  import scala.collection.mutable.{ Set => MutSet }

  case class IH (i: Int, h: Int) {
    override def hashCode: Int = h
  }

  def run(): Unit = {
    var is = ImmutSet.empty[IH]
    var ms = MutSet.empty[IH]
    for (ih <- List(IH(2,0),IH(0,0),IH(4,4),IH(6,4),IH(-8,1520786080))) {
      is = is + ih
      ms = ms ++ List(ih)
    }
    assert(is == ms)
    val x = IH(6,4)
    is = is - x
    ms = ms.filter(e => e != x)
    assert(is == ms)
  }
}

object MapBug {
  import scala.collection.immutable.{ Map => ImmutMap }
  import scala.collection.mutable.{ Map => MutMap }

  case class IH (i: Int, h: Int) {
    override def hashCode: Int = h
  }

  def run(): Unit = {
    var im = ImmutMap.empty[IH,IH]
    var mm = MutMap.empty[IH,IH]
    for (ih <- List(IH(2,0),IH(0,0),IH(4,4),IH(6,4),IH(-8,1520786080))) {
      im = im + ((ih,ih))
      mm = mm ++ List((ih,ih))
    }
    assert(im == mm)
    val x = IH(6,4)
    im = im - x
    mm = mm.view.filterKeys(k => k != x).to(MutMap)
    assert(im == mm)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    SetBug.run()
    MapBug.run()
  }
}
