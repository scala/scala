class Sync

class Async extends Sync { def x = 1 }

trait Base {
  def foo: Sync
}

trait BaseSync extends Base {
  override def foo: Sync
}

trait BaseAsync extends Base  {
  override def foo: Async
}

abstract class ImplAsync extends BaseAsync {
  final override def foo: Async = new Async
}

final class StrangeClass extends ImplAsync with BaseSync

object Test {
  def main(args: Array[String]): Unit = {
    assert((new StrangeClass).foo.x == 1)
  }
}