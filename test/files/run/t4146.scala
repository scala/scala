object bob extends Application {
  var name = "Bob"
}

object Test extends App {
  assert(bob.name == "Bob")
}
