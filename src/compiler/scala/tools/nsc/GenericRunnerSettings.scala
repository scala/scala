package scala.tools.nsc

class GenericRunnerSettings(error: String=>Unit)
extends Settings(error) {
  val howtorun =
    ChoiceSetting(
      "howtorun",
      "how to run the specified code",
      List("guess", "object", "script"),
      "guess")
}
