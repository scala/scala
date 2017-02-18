package dynamicrash

import scala.language.dynamics

class Config

trait Extractor[A] {
  def extract(config: Config, name: String): A
}

object Extractor {
  // note missing "implicit"
  val stringExtractor = new Extractor[String] {
    override def extract(config: Config, name: String): String = ???
  }
}

class Workspace extends Dynamic {
  val config: Config = new Config

  def selectDynamic[A](name: String)(implicit extractor: Extractor[A]): A =
    extractor.extract(config, name)
}

object Main {
  val storage = new Workspace

  // this line works fine
  // val a = storage.foo

  // this line crashes the compiler ("head of empty list")
  // in ContextErrors$InferencerContextErrors$InferErrorGen$.NotWithinBoundsErrorMessage
  println(storage.foo[String])

  // this line crashes the compiler in different way ("unknown type")
  // in the backend, warning: an unexpected type representation reached the compiler backend while compiling Test.scala: <error>
  println(storage.foo)
}
