package tastytest

object DelayedExtends {

  final class Foo extends Internal.Bar[FinalA.Inner.LeafA]

  object Internal {
    class Bar[T]
  }

  object Lvl1 {
    object Lvl2 {
      object Lvl3 {
        sealed abstract class Branch
      }
    }
  }

  object FinalA {
    object Inner {
      final class LeafA extends Lvl1.Lvl2.Lvl3.Branch
    }
  }

  object FinalB {
    object Inner {
      final class LeafB extends Lvl1.Lvl2.Lvl3.Branch
    }
  }

}
