class TypeTreeObjects {
  class Container {
    def typeParamAndDefaultArg[C](name: String = ""): String = ""
  }
  // crashed under -Yrangepos
  new Container().typeParamAndDefaultArg[Any]()
}
