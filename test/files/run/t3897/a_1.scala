//> using options -Ydelambdafy:inline
class One {
  private val messages = new collection.mutable.ListBuffer[String]
  List("a") foreach { messages += _ }
}

class Two {
  private val messages = new collection.mutable.ListBuffer[String]
}
