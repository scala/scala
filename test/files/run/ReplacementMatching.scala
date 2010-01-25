







object Test {

  def main(args: Array[String]) {
    val regex = """\$\{(.+?)\}""".r
    val replaced = regex.replaceAllMatchDataIn("Replacing: ${main}. And another method: ${foo}.",
        (m: util.matching.Regex.Match) => {
      val identifier = m.group(1)
      identifier
    })
    assert(replaced == "Replacing: main. And another method: foo.")

    val regex2 = """\$\{(.+?)\}""".r
    val replaced2 = regex2.replaceAllIn("Replacing: ${main}. And then one more: ${bar}.", (s: String) => {
      "$1"
    })
    assert(replaced2 == "Replacing: main. And then one more: bar.")
  }

}
