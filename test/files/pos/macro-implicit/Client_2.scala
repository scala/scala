
class Client {
  def testV = {
    import Macro.v
    new C().rich
  }
  def testM = {
    import Macro.m
    implicitly[String]
  }

}
