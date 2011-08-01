import scala.util.Properties._
import scala.tools.nsc.util.ClassPath._

object Test {
  def main(args: Array[String]): Unit = {
    infoFor(this).jarManifestMainAttrs get ScalaCompilerVersion match {
      case Some(v) => println("I was built by scala compiler version " + v)
      case _       => println("I was not apprised of which scala compiler version built me.")
    }
  }
}
