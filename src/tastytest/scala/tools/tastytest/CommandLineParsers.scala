package scala.tools.tastytest

import scala.util.Try

object CommandLineParsers {

  def optionalArg(arg: String, default: => String)(implicit args: Seq[String]): String =
    findArg(arg).getOrElse(default)

  def requiredArg(arg: String)(implicit args: Seq[String]): Try[String] =
    findArg(arg).failOnEmpty(new IllegalArgumentException(s"please provide argument: $arg"))

  def booleanArg(arg: String)(implicit args: Seq[String]): Boolean =
    args.contains(arg)

  def findArg(arg: String)(implicit args: Seq[String]): Option[String] =
    args.sliding(2).filter(_.length == 2).find(_.head == arg).map(_.last)

}
