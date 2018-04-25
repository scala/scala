
class Test {
  val cm: reflect.runtime.universe.Mirror = reflect.runtime.currentMirror
  def error: Unit = {
    new cm.universe.Traverser // java.lang.VerifyError: (class: Test, method: error signature: ()V) Incompatible object argument for function call

  }

  def okay1: Unit = {
    val cm: reflect.runtime.universe.Mirror = reflect.runtime.currentMirror

    new cm.universe.Traverser
  }

  def okay2: Unit = {
    val cm: reflect.runtime.universe.Mirror = reflect.runtime.currentMirror
    val u: reflect.runtime.universe.type = cm.universe
    new u.Traverser
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new Test().error
    new Test().okay1
    new Test().okay2
  }
}
