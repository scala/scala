// not exactly "pending", here as an example usage.
//
val manager = scala.tools.util.SignalManager

manager.requireInterval(3, manager.INT) {
  case true   => Console.println("\nPress ctrl-C again to exit.")
  case false  => System.exit(1)
}

manager("HUP") = println("HUP 1!")
manager("HUP").raise()

manager("HUP") += println("HUP 2!")
manager("HUP").raise()

manager("HUP") += println("HUP 3!")
manager("HUP").raise()

manager("HUP") = println("Back to HUP 1!")
manager("HUP").raise()

manager.dump()
