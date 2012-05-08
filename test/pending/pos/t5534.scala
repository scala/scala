object Phrase extends Enumeration {
  type Phrase = Value
  val PHRASE1 = Value("My phrase 1")
  val PHRASE2 = Value("My phrase 2")
}

class Entity(text:String)

object Test {
    val myMapWithPhrases = Phrase.values.map(p => (p -> new Entity(p.toString))).toMap
}