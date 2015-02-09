import scala.language.{ higherKinds, implicitConversions }

trait Foo { type Out }

trait SI {
  val instance: Foo
  type Out
}

object Test {
  def test {
    def indirect(si: SI)(v: si.instance.Out) = v

    val foo: Foo { type Out = Int } = ???
    def conv(i: Foo): SI { type Out = i.Out; val instance: i.type } = ???

    val converted = conv(foo)

    val v1: Int = indirect(converted)(23)  // Okay (after refining the return type `instance` in the return type of `conv`)
    /*
    indirect(converted){(v: converted.instance.Out)converted.instance.Out}(
      23{Int(23)}
    ){converted.instance.Out};
    */

    val v2: Int = indirect(conv(foo))(23)  // Used to fail as follows:
    /*
    indirect(
        conv(foo){si.SI{type Out = foo.Out; val instance: si.Test.<refinement>.type}}
    ){(v: si.instance.Out)si.instance.Out}(
      23{<error>}
    ){<error>};
    */

  }
}
