class C {
  var ORef: Object = null
  def test = {
    object O {
      assert(!Thread.holdsLock(C.this))
      assert(Thread.holdsLock(ORef))
    }
    val captor = new { def oh = O }
    val refField = captor.getClass.getDeclaredFields.last
    refField.setAccessible(true)
    assert(refField.getType.toString.contains("LazyRef"), refField)
    ORef = refField.get(captor)
    O
  }
}

class D {
  var ORef: Object = null
  def test = {
    lazy val O = {
      assert(!Thread.holdsLock(D.this))
      assert(Thread.holdsLock(ORef))
      "O"
    }
    val captor = new { def oh = O }
    val refField = captor.getClass.getDeclaredFields.last
    refField.setAccessible(true)
    assert(refField.getType.toString.contains("LazyRef"), refField)
    ORef = refField.get(captor)
    O
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new C().test
    new D().test
  }
}
