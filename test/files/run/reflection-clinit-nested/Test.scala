import reflect.runtime._

object Test {
  def main(args: Array[String]): Unit = {
    currentMirror.staticClass("p1.A").info.members.find(_.isType).get.name
    currentMirror.staticModule("p1.A").info.members.find(_.isType).get.name
  }
}
