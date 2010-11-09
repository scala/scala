object Test {
  import scala.collection.immutable.{ Set => ImmutSet }
  import scala.collection.mutable.{ Set => MutSet }

  case class IH (i: Int, h: Int) {
    override def hashCode: Int = h
  }

  def main (args: Array[String]) {
    var is = ImmutSet.empty[IH]
    var ms = MutSet.empty[IH]
    for (ih <- List(IH(2,0),IH(0,0),IH(4,4),IH(6,4),IH(-8,1520786080))) {
      is = is + ih
      ms = ms + ih
    }
    val x = IH(6,4)
    is = is - x
    ms = ms - x
    assert(is == ms)
  }
}
