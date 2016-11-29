// This test illustrates the intent of what should work (but didn't for a while during the fields refactoring),
// but it does not actually defend against the regression seen in twitter-util's Scheduler, which I cannot reproduce
// outside the project. The whole project consistently fails to build before, and compiles after the commit
// that includes this test, but this single test file (as well as Scheduler.scala with external dependencies removed)
// compiles both before and after....
// (https://github.com/twitter/util/blob/6398a56923/util-core/src/main/scala/com/twitter/concurrent/Scheduler.scala#L260-L265)
// There's also a run test that checks that the field in C is actually volatile.
trait Vola {
  @volatile private[this] var _vola = "tile"
  @volatile var vola = "tile"
}

class C extends Vola
