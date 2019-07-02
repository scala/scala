
trait MyAny {
  type TSomething
  type This <: MyAny
}

class MyBool extends MyAny {
  type TSomething = DummyImplicit
  type This = MyBool
}

object Test extends App {
  val i = new MyBool
  val j = new MyBool
  var call = 0

  final class MyMatch[MV <: MyAny](val matchVal : MV)  {
    def myCase[MC](pattern : Boolean)(block : => Unit)(
      implicit patternBld : matchVal.TSomething
    ) : MyMatch[MV] = {
      call -= 1
      block
      this
    }
  }

  def myMatch[MV <: MyAny](matchValue : MV) : MyMatch[matchValue.This] = {
    assert(call == 0)
    call += 1
    new MyMatch[matchValue.This](matchValue.asInstanceOf[matchValue.This])
  }

  myMatch(i)
    .myCase(true) {
      //val x = 42
      myMatch(j)
        .myCase(true) {

        }
    }
}
