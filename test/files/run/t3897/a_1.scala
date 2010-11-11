class One {
  private val messages = new collection.mutable.MutableList[String]
  List("a") foreach { messages += _ }
}

class Two {
  private val messages = new collection.mutable.MutableList[String]
}
