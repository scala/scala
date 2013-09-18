object Weird {
  { (s: String) =>
      val foo = Some(s); // to illustrate that vals are printed in the error
      foo
  }.apply("first param") ("spurious param")
}