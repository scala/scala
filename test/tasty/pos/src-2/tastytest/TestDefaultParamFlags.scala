package tastytest

class DefaultParamFlagsScala2 {
  def method(a: Int, b: Int = 1): Int = a + b
}

object TestDefaultParamFlags {

  compiletimeTestDefaultParamFlags[DefaultParamFlags]()
  compiletimeTestDefaultParamFlags[DefaultParamFlagsScala2]()

}
