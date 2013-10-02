import sys._

/** Basic sys.Prop test. */
object Test {
  val key = "ding.dong.doobie"

  def bool() = {
    val prop = BooleanProp.valueIsTrue(key)
    assert(prop.key == key)

    prop.clear()
    assert(!prop.value)
    assert(!prop.isSet)
    assert(prop.get != null)

    prop set "dingus"
    assert(prop.get == "dingus")
    assert(!prop.value)
    prop set "true"
    assert(prop.value)
    prop.toggle()
    assert(!prop.value)
    prop.enable()
    assert(prop.value)
    prop.disable()
    assert(!prop.value)
  }
  def int() = {
    val prop = Prop[Int](key)
    prop.clear()
    assert(prop.value == 0)
    prop.set("523")
    assert(prop.value == 523)
    prop.set("DingusInt")

    try { println(prop.value) ; assert(false, "should not get here") }
    catch { case _: Exception => () }
  }
  def double() = {
    val prop = Prop[Double](key)
    prop.set("55.0")
    assert(prop.value == 55.0)
  }

  def main(args: Array[String]): Unit = {
    bool()
    int()
    double()
  }
}
