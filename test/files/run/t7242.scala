class CrashTest {
  def foo = ()
  trait CrashTestTable {
    def cols = foo
  }
  // This was leading to a class between the mixed in
  // outer accessor and the outer accessor of this object.
  object CrashTestTable extends CrashTestTable {
    foo
    cols
  }
}

class CrashTest1 {
  def foo = ()
  class CrashTestTable {
    def cols = foo
  }
  object CrashTestTable extends CrashTestTable {
    foo
    cols
  }
}

class CrashTest2 {
  def foo = ()
  trait CrashTestTable {
    def cols = foo
  }
  object Obj extends CrashTestTable {
    foo
    cols
  }
}

class CrashTest3 {
  def foo = ()

  def meth() {
    trait CrashTestTable {
      def cols = foo
    }
    object Obj extends CrashTestTable {
      foo
      cols
    }
    Obj
  }
}

object Test extends App {
  {
    val c = new CrashTest
    c.CrashTestTable
  }

  {
    val c = new CrashTest1
    c.CrashTestTable
  }

  {
    val c = new CrashTest2
    c.Obj
  }

  {
    val c = new CrashTest3
    c.meth()
  }
}
