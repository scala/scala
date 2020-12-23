package tastytest

/** test calling $init$ on IOApp parent */
object TestIOApp extends Suite("TestIOApp") {

  def randOver5 = scala.util.Random.nextInt(5) + 5

  object Static extends IOApp.Simple {
    def run = assert(randOver5 >= 5)
  }

  test("static IOApp")(Static.run)

  test("local IOApp") {
    val Local = new IOApp.Simple {
      def run = assert(randOver5 >= 5)
    }
    Local.run
  }


}
