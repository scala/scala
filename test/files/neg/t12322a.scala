class Wrapper {
  object MyObj
  val obj = new MyObj.type

  type Oops = MyObj.type
  val oops = new Oops
}
