/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.scalap

import scala.collection.mutable
import mutable.ListBuffer

object Arguments {
  case class Parser(optionPrefix: Char) {
    val options: mutable.Set[String]                = new mutable.HashSet
    val prefixes: mutable.Set[String]               = new mutable.HashSet
    val optionalArgs: mutable.Set[String]           = new mutable.HashSet
    val prefixedBindings: mutable.Map[String, Char] = new mutable.HashMap
    val optionalBindings: mutable.Map[String, Char] = new mutable.HashMap

    def argumentError(message: String): Unit = Console.println(message)

    def withOption(option: String): Parser = {
      options += option
      this
    }

    def withOptionalArg(option: String): Parser = {
      optionalArgs += option
      this
    }

    def withOptionalBinding(option: String, separator: Char): Parser = {
      optionalBindings(option) = separator
      this
    }

    def withPrefixedArg(prefix: String): Parser = {
      prefixes += prefix
      this
    }

    def withPrefixedBinding(prefix: String, separator: Char): Parser = {
      prefixedBindings(prefix) = separator
      this
    }

    def parseBinding(str: String, separator: Char): (String, String) = (str indexOf separator) match {
      case -1   => argumentError(s"missing '$separator' in binding '$str'") ; ("", "")
      case idx  => ((str take idx).trim, (str drop (idx + 1)).trim)
    }

    def parse(args: Array[String]): Arguments = {
      val res = new Arguments
      parse(args, res)
      res
    }

    def parse(args: Array[String], res: Arguments): Unit = {
      if (args != null) {
        var i = 0
        while (i < args.length)
          if ((args(i) == null) || (args(i).length() == 0))
            i += 1
          else if (args(i).charAt(0) != optionPrefix) {
            res.addOther(args(i))
            i += 1
          } else if (options(args(i))) {
            res.addOption(args(i))
            i += 1
          } else if (optionalArgs contains args(i)) {
            if ((i + 1) == args.length) {
              argumentError(s"missing argument for '${args(i)}'")
              i += 1
            } else {
              res.addArgument(args(i), args(i + 1))
              i += 2
            }
          } else if (optionalBindings contains args(i)) {
            if ((i + 1) == args.length) {
              argumentError(s"missing argument for '${args(i)}'")
              i += 1
            } else {
              res.addBinding(args(i),
                parseBinding(args(i + 1), optionalBindings(args(i))))
              i += 2
            }
          } else {
            val iter = prefixes.iterator
            val j = i
            while ((i == j) && iter.hasNext) {
              val prefix = iter.next()
              if (args(i) startsWith prefix) {
                res.addPrefixed(prefix, args(i).substring(prefix.length()).trim())
                i += 1
              }
            }
            if (i == j) {
              val iter = prefixedBindings.keysIterator
              while ((i == j) && iter.hasNext) {
                val prefix = iter.next()
                if (args(i) startsWith prefix) {
                  val arg = args(i).substring(prefix.length()).trim()
                  i = i + 1
                  res.addBinding(prefix,
                    parseBinding(arg, prefixedBindings(prefix)))
                }
              }
              if (i == j) {
                argumentError(s"unknown option '${args(i)}'")
                i = i + 1
              }
            }
          }
      }
    }
  }

  def parse(options: String*)(args: Array[String]): Arguments = {
    val parser = new Parser('-')
    options foreach parser.withOption
    parser parse args
  }
}

class Arguments {
  private val options   = new mutable.HashSet[String]
  private val arguments = new mutable.HashMap[String, String]
  private val prefixes  = new mutable.HashMap[String, mutable.HashSet[String]]
  private val bindings  = new mutable.HashMap[String, mutable.HashMap[String, String]]
  private val others    = new ListBuffer[String]

  def addOption(option: String): Unit = options += option

  def addArgument(option: String, arg: String): Unit = arguments(option) = arg

  def addPrefixed(prefix: String, arg: String): Unit =
    prefixes.getOrElseUpdate(prefix, new mutable.HashSet) += arg

  def addBinding(tag: String, key: String, value: String): Unit =
    if (key.length > 0)
      bindings.getOrElseUpdate(tag, new mutable.HashMap)(key) = value

  def addBinding(tag: String, binding: (String, String)): Unit =
    addBinding(tag, binding._1, binding._2)

  def addOther(arg: String): Unit = others += arg

  def contains(option: String): Boolean = options(option)

  def getArgument(option: String): Option[String] = arguments get option

  def getSuffixes(prefix: String): mutable.Set[String] =
    prefixes.getOrElse(prefix, new mutable.HashSet)

  def containsSuffix(prefix: String, suffix: String): Boolean =
    prefixes get prefix exists (set => set(suffix))

  def getBindings(tag: String): mutable.Map[String, String] =
    bindings.getOrElse(tag, new mutable.HashMap)

  def getBinding(option: String, key: String): Option[String] =
    bindings get option flatMap (_ get key)

  def getOthers: List[String] = others.toList
}
