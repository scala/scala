// https://github.com/sbt/sbt/issues/3736
// https://github.com/raboof/sbt-run-classloading/blob/master/src/main/scala/Main.scala
object Main extends App {
  Class.forName("scala.Int")

  val classLoader = Option(Thread.currentThread.getContextClassLoader).get
  Class.forName("scala.Int", true, classLoader)

  Class.forName("scala.Int", false, classLoader)
}
