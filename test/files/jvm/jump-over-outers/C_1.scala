
// tests one of the transformations performed in the constructors phase,
// (shortening navigation-paths over outer-instances to minimize heap retention at runtime)

// The anon-closure below should be emitted not with an outer-field holding a MyActor value
// but a C_1 value (the nearmost outer-instance that's actually needed, in the example to access field `name`)

class C_1 {
  var name = "abc"
  class MyActor {
    def doSomeWork() {
      (1 to 10) foreach { i => println(name) }
    }
  }
}

