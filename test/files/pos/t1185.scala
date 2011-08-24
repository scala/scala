class Test {
  private[this] var member = 0;
  def foo() = {
    (() => member=1)()
  }
  def look=member
}

object Main{
  def main(args : Array[String]){
    val fff=new Test()
    fff.foo()
    assert(1==fff.look)
  }
}
