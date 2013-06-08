
// tests one of the transformations performed in the constructors phase,
// (shortening navigation-paths over outer-instances to minimize heap retention at runtime)

// The inner class below should be emitted not with an outer-field holding a MyActor value
// but a C_1 value (the nearmost outer-instance that's actually needed, in the example to access field `name`)
// Invocations to MyActor.xyz() from Inn's auxiliary and primary constructors don't count towards a direct-outer field in Inn pointing to a MyActor instance.

class C_1 {
  var name = "abc"
  class MyActor {

    def xyz() { println("act()") }

    final class Inn {

      xyz()

      def this(i: Int) { this(); xyz() }

      def doSomeWork() {
        println(name)
      }

    }

  }
}

