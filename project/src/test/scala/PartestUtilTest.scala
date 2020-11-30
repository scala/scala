package scala.build

import sbt._
import sbt.complete._, Completion._

import scala.Console.{ GREEN, RED, RESET, err }

// This doesn't get run automatically
// but it was handy when tweaking PartestUtil
// so it may be handy down the line.
//
// Run it manually like so:
//      > reload plugins
//      ...
//      [info] loading project definition from /d/scala/project
//      > Test/runMain scala.build.PartestUtilTest
//      [info] running scala.build.PartestUtilTest
//
//      = base=/d/scala
//      = path=pos/t177
//      + completions partest pos/t177 OK
//      + completions partest test/files/pos/t177 OK
//      ...
object PartestUtilTest {
  def main(args: Array[String]): Unit = {
    def test(base: File, str: String, expected: Completions) = {
      print(s"+ completions partest $str ")
      val P = PartestUtil.partestParser(base, base / "test")
      val completions = Parser.completions(P, s" $str", 9).filterS(_.nonEmpty)
      def pp(c: Completions, pre: String) = c.get.iterator.map(x => s"\n$pre  $x)$RESET").mkString
      def ppStr = s"($RED-obtained$RESET/$GREEN+expected$RESET):${pp(completions, s"$RED-")}${pp(expected, s"$GREEN+")}"
      if (completions == expected) {
        println(s"${GREEN}OK$RESET")
        //println(s"Completions $ppStr")
      } else {
        println(s"${RED}KO$RESET")
        err.println(s"Completions $ppStr")
        throw new AssertionError("Completions mismatch")
      }
    }

    def prependTokens(completions: Completions, prefix: String) = completions.map {
      case t: Token => new Token(s"$prefix${t.display}", t.append)
      case x        => x
    }

    def testGen(base: File, path: String, res: Completions) = {
      println(s"= path=$path")
      test(base, s"$path",                  prependTokens(res, ""))
      test(base, s"test/files/$path",       prependTokens(res, "test/files/"))
      test(base, s"./test/files/$path",     prependTokens(res, "./test/files/"))
      if (base.isAbsolute) // a prerequisite as this is to test absolute path tab-completion
        test(base, s"$base/test/files/$path", prependTokens(res, s"$base/test/files/"))
    }

    def tests(base: File) = {
      println(s"\n= base=$base")
      testGen(base, "pos/t177",        Completions(Set(token("pos/t177", ".scala"))))
      testGen(base, "pos/t177.scala",  Completions(Set(suggestion(" "))))
      testGen(base, "pos/t1786",       Completions(Set(token("pos/t1786", "-counter.scala"), token("pos/t1786", "-cycle.scala"))))
      testGen(base, "pos/t1786.scala", Completions(Set()))
    }

    tests(file(sys.props("user.dir")))
    tests(file(".").getAbsoluteFile)
    tests(file("."))
    tests(file("./"))
  }
}
