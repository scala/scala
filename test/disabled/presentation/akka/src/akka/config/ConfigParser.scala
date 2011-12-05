/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 *
 * Based on Configgy by Robey Pointer.
 *   Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *   http://www.apache.org/licenses/LICENSE-2.0
 */

package akka.config

import scala.collection.mutable
import scala.util.parsing.combinator._

class ConfigParser(var prefix: String = "", map: mutable.Map[String, Any] = mutable.Map.empty[String, Any], importer: Importer) extends RegexParsers {
  val sections = mutable.Stack[String]()

  def createPrefix = {
    prefix = if (sections.isEmpty) "" else sections.toList.reverse.mkString("", ".", ".")
  }

  override val whiteSpace = """(\s+|#[^\n]*\n)+""".r

  // tokens

  val numberToken: Parser[String] = """-?\d+(\.\d+)?""".r
  val stringToken: Parser[String] = ("\"" + """([^\\\"]|\\[^ux]|\\\n|\\u[0-9a-fA-F]{4}|\\x[0-9a-fA-F]{2})*""" + "\"").r
  val booleanToken: Parser[String] = "(true|on|false|off)".r
  val identToken: Parser[String] = """([\da-zA-Z_][-\w]*)(\.[a-zA-Z_][-\w]*)*""".r
  val assignToken: Parser[String] = "=".r
  val sectionToken: Parser[String] = """[a-zA-Z][-\w]*""".r

  // values

  def value: Parser[Any] = number | string | list | boolean
  def number = numberToken
  def string = stringToken ^^ { s => s.substring(1, s.length - 1) }
  def list = "[" ~> repsep(string | numberToken, opt(",")) <~ (opt(",") ~ "]")
  def boolean = booleanToken

  // parser

  def root = rep(includeFile | assignment | sectionOpen | sectionClose)

  def includeFile = "include" ~> string ^^ {
    case filename: String =>
      new ConfigParser(prefix, map, importer) parse importer.importFile(filename)
  }

  def assignment = identToken ~ assignToken ~ value ^^ {
    case k ~ a ~ v => map(prefix + k) = v
  }

  def sectionOpen = sectionToken <~ "{" ^^ { name =>
    sections push name
    createPrefix
  }

  def sectionClose = "}" ^^ { _ =>
    if (sections.isEmpty) {
      failure("dangling close tag")
    } else {
      sections.pop
      createPrefix
    }
  }

  def parse(in: String): Map[String, Any] = {
    parseAll(root, in) match {
      case Success(result, _) => map.toMap
      case x@Failure(msg, _)  => throw new ConfigurationException(x.toString)
      case x@Error(msg, _)    => throw new ConfigurationException(x.toString)
    }
  }
}
