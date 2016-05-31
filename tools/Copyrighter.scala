/*
 * Copyright 2016 We don't need no stinkin' copyright notices.
 */
import java.nio.file._
import java.time._

import scala.collection.mutable

object Copyrighter extends App {

  val usage = "Usage: scala -nc tools/Copyrighter.scala [--follow|--update|--verbose|--help]"

  val config = args.foldLeft(Set.empty[String])((flags, arg) => arg match {
    case "--update" | "--follow" | "--verbose" => flags + arg
    case "--help" | _ => Console.err.println(usage) ; sys.exit(1)
  })

  val app = new Copyrighter(config.contains("--verbose"))
  if (config.contains("--follow"))
    app.follow(config.contains("--update"))
  else
    app.verify(config.contains("--update"))
}

class Copyrighter(verbose: Boolean) {
  import EasyOut._

  // string regex for what looks like a copyright
  val copyrightish = raw"(\(c\)|Copyright) 20[0-9]{2}(-(20[0-9]{2}))?"

  // regex for copyright dates, with capturing groups for start and end year
  val copyregex = raw"(\(c\)|Copyright) (20[0-9]{2})(?:-(20[0-9]{2}))?".r.unanchored

  case class Copyright(path: Path, lineno: Int, text: String)

  case class Dated(path: Path, from: Year, to: Option[Year])

  /** Sources that seem to bear a copyright, as (Path, line number, text) */
  def copyrighted: List[Copyright] = {
    val gitted = xs"git grep -n -E -e $copyrightish -- *.scala"
    val parted = gitted.map { line =>
      val parts = line.split(":", 3)
      Copyright(Paths.get(parts(0)), parts(1).toInt, parts(2))
    }
    parted.toList
  }
  def copyrightedS: Stream[(Path, Int, String)] = {
    val gitted = xs"git grep -n -E -e $copyrightish -- *.scala"
    gitted.map { line =>
      val parts = line.split(":", 3)
      (Paths.get(parts(0)), parts(1).toInt, parts(2))
    }
  }

  /*
   * Log all the paths.
   *
   * Fast method can't follow, so won't correctly detect earliest commit of code
   * through moves. For example, `scala.App` should have the same start year as
   * `scala.Application` from which it is derived.
   *
   * "usage: git logs can only follow renames on one pathname at a time"
   */
  def verify(update: Boolean): Unit = {
    def rangify(info: Iterator[String]): Map[Path, Dated] = {
      val res = mutable.Map.empty[Path, Dated]
      val lines = info
      var lastYear: Year = Year.of(Year.MIN_VALUE)
      while (lines.hasNext) {
        var yearly = lines.next()
        while (yearly.isEmpty && lines.hasNext) yearly = lines.next()
        if (lines.hasNext) {
          val year = Year.parse(yearly.substring(0, 4))
          if (verbose && year != lastYear) {
            pe"Year $year"
            lastYear = year
          }
          var line = lines.next()
          assert(line.isEmpty)
          do {
            line = lines.next()
            if (line.nonEmpty) {
              val p = Paths.get(line)
              res.get(p) match {
                case Some(dated) => res.put(p, dated.copy(from = year))
                case None        => res.put(p, Dated(p, from = year, to = Some(year)))
              }
              //res.put(p, Dated(p, year, y)).foreach(dated => res.update(p, dated.copy(to = y)))
            }
          } while (line.nonEmpty && lines.hasNext)
        }
      }
      res.toMap
    }
    def logged(candidates: List[Copyright]): Map[Path, Dated] = {
      val info = xx"git log --no-merges --date=short --format=%n%cd --name-only"(candidates.map(_.path.toString))
      rangify(info.iterator)
    }
    val limit = 10
    val candidates = copyrighted.filter(_.lineno <= limit)
    val ranged = logged(candidates)

    for (candidate @ Copyright(path, lineno, text) <- candidates) {
      text match {
        case copyregex(_, start, end) =>
          val notice = Dated(path, Year.parse(start), Option(end).map(Year.parse))
          val actual = ranged(path)
          if (!check(notice, actual) && update && !fix(candidate, actual)) ps"$path: Unable to fix. :("
      }
    }
  }

  /** Check whether notice conforms to actual. True for OK.
   *
   *  Verify that start in notice is not after first commit,
   *  and end in notice is not before last commit.
   */
  def check(notice: Dated, actual: Dated): Boolean = {
    val badStart = actual.from isBefore notice.from
    val badUntil = notice.to.map(_ isBefore actual.to.get).getOrElse(false)
    val warnUntil = notice.to.map(_ isAfter actual.to.get).getOrElse(false)
    val failed = badStart || badUntil || warnUntil
    if (failed) {
      if (verbose) {
        val starting = if (badStart) s"Bad from ${notice.from} should be ${actual.from}. " else ""
        val ending = notice.to match {
          case Some(to) =>
            if (badUntil) s"Bad to ${to} should be ${actual.to.get}."
            else if (warnUntil) s"Warning: to ${to} is after last commit in ${actual.to.get}."
            else ""
          case None     => s"Missing to should be ${actual.to.get}"
        }
        ps"${notice.path}: $starting$ending"
      } else {
        val starting = if (badStart) s"${notice.from}->${actual.from}" else s"${notice.from}"
        val ending = notice.to match {
          case Some(to) =>
            if (badUntil) s"${to}->${actual.to.get}"
            else if (warnUntil) s"${to}~>${actual.to.get}"
            else s"${to}"
          case None     => s"?->${actual.to.get}"
        }
        ps"${notice.path}: ($starting, $ending)"
      }
    }
    !failed
  }

  /** Attempt to update the candidate path with actual years.
   */
  def fix(candidate: Copyright, actual: Dated): Boolean = {
    import scala.collection.JavaConverters._
    import scala.io.Codec.UTF8.{ charSet => cs }
    import candidate._, actual.{ from, to }
    val lines = Files.readAllLines(path, cs).asScala
    assert(lines(lineno - 1) == text)
    val before = lines.take(lineno - 1)
    val after = lines.drop(lineno)
    val corrected: String = copyregex.replaceAllIn(text, m => s"${m.group(1)} ${from}-${to.get}")
    val w = Files.newBufferedWriter(path, cs)
    def outln(s: String) = {
      w.write(s)
      w.newLine()
    }
    try {
      before.foreach(outln)
      outln(corrected)
      after.foreach(outln)
      true
    } catch {
      case e: java.io.IOException =>
        e.printStackTrace()
        false
    } finally w.close
  }

  /** Slow version gets history per path, in order to follow moves. */
  def follow(update: Boolean): Unit = {
    val limit = 10
    val candidates = copyrighted.filter(_.lineno <= limit)

    for (candidate @ Copyright(path, lineno, text) <- candidates) {
      text match {
        case copyregex(_, start, end) =>
          def yearOf(log: String) = Year.parse(log.substring(0, 4))
          val notice = Dated(path, Year.parse(start), Option(end).map(Year.parse))
          val gitted = xs"""git log --no-merges --follow --date=short --format=%cd $path"""
          val actual = Dated(path, yearOf(gitted.last), gitted.headOption.map(yearOf))
          if (!check(notice, actual) && update && !fix(candidate, actual)) ps"$path: Unable to fix. :("
      }
    }
  }
}

object EasyOut {
  /*
  private[this] val inquotes = """(['"])(.*?)\1""".r
  def unquoted(s: String) = s match { case inquotes(_, w) => w ; case _ => s }
  def words(s: String) = (s.trim split "\\s+" filterNot (_ == "") map unquoted).toList
  */

  implicit class `easy println`(val sc: StringContext) extends AnyVal {
    import StringContext.treatEscapes, scala.runtime.ScalaRunTime.stringOf
    /** Print stringOf args. */
    def p(args: Any*): String = {
      val s = sc.standardInterpolator(treatEscapes, args.map(stringOf))
      println(s)
      s
    }
    /** out.print args.toString. */
    def ps(args: Any*): String = {
      val s = sc.standardInterpolator(treatEscapes, args.map(_.toString))
      println(s)
      s
    }
    /** err.print args.toString. */
    def pe(args: Any*): String = {
      val s = sc.standardInterpolator(treatEscapes, args.map(_.toString))
      Console.err.println(s)
      s
    }
  }
  implicit class `easy process`(val sc: StringContext) extends AnyVal {
    import StringContext._, scala.runtime.ScalaRunTime.stringOf
    def sh(args: Any*): String = {
      import StringContext.processEscapes
      import sys.process._
      sc.checkLengths(args)
      val ss = sc.parts.zipAll(args, "", "").flatMap {
        case (s, a) => processEscapes(s).split("\\s+").toList.filter(_.nonEmpty) :+ a.toString
      }.init
      //p"Try: $ss"
      //ss.!!
      ???
    }
    /** Execute a process.
     *
     *  String parts are split on spaces to form elements of the command;
     *  empty parts are ignored; interpolated args are taken for elements.
     *
     *  No quote parsing is performed: use interpolated arg for embedded spaces and empty args.
     *
     *  Use `x` for `String` result, `xs` for `Stream[String]` and `xx` for
     *  a function that takes more args before running the command.
     *
     *  val dir = Paths.get("\\Program Files").toFile
     *  x"ls -ltr $dir" => Process(Seq("ls", "-ltr", dir.toString))
     *  x"$cmd$option$dir" => Process(Seq(cmd.toString, option.toString, dir.toString))
     *
     *  val empty = ""
     *  x"scalac -cp $empty *.scala" => Process(Seq("scalac", "-cp", "", "*.scala"))
     */
    def x(args: Any*): String = {
      import sys.process._
      seq(args: _*).!!
    }
    def xs(args: Any*): Stream[String] = {
      import sys.process._
      seq(args: _*).lineStream
    }
    def xx(args: Any*): Seq[String] => Stream[String] = {
      import sys.process._
      val ss = seq(args: _*)
      more => (ss ++ more).lineStream
    }
    def seq(args: Any*): Seq[String] = {
      import StringContext.processEscapes
      sc.checkLengths(args)
      sc.parts.zipAll(args, "", "").flatMap {
        case (s, a) => processEscapes(s).split("\\s+").toList.filter(_.nonEmpty) :+ a.toString
      }.init
    }
  }
}
