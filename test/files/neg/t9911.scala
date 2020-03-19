// This should say:
// Error: super may not be used on value source
class ScalacBug {

  class SomeClass {

    type U

    // Changing T or U stops the problem
    def getSomething[T]: U = ???
  }

  trait Base {

    // Changing this to a def like it should be stops the problem
    val source: SomeClass = ???
  }

  class Bug extends Base {

    override val source = {
      // Not calling the function stops the problem
      super.source.getSomething
      ???
    }
  }

}
