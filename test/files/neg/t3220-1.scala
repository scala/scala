object Example {
  val badsingle = "foo \unope that's wrong"
  val badtriple = """foo \unope that's wrong""" 
  val caretPos = "foo \u12x3 pos @ x"
  val caretPos2 = "foo \uuuuuuu12x3 pos @ x"
  val carPosTerm = "foo \u123"
  val halfAnEscape = "foo \u12"
  val halfAnEscapeChar = '\u45'
  val `half An Identifier\u45` = "nope"
}