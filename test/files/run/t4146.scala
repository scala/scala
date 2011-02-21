object bob extends Application {
  var name = "Bob"
}

object Test extends Application {
  assert(bob.name == "Bob")
}
