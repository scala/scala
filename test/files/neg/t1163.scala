trait Super { def foo: Super = new Super {} }
trait Sub extends Super { override def foo: Sub }
