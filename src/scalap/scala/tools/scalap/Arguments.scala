/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2006, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
*/

// $Id$

package scala.tools.scalap

import scala.collection.mutable._


object Arguments {

  case class Parser(optionPrefix: Char) {

    val options: Set[String] = new HashSet
    val prefixes: Set[String] = new HashSet
    val optionalArgs: Set[String] = new HashSet
    val prefixedBindings: Map[String, Char] = new HashMap
    val optionalBindings: Map[String, Char] = new HashMap

    def error(message: String): Unit = Console.println(message)

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

    def parseBinding(str: String, separator: Char): Pair[String, String] = {
      val eqls = str.indexOf(separator)
      if (eqls < 0) {
        error("missing '" + separator + "' in binding '" + str + "'")
        Pair("", "")
      } else
        Pair(str.substring(0, eqls).trim(),
           str.substring(eqls + 1).trim())
    }

    def parse(args: Array[String]): Arguments = {
      val res = new Arguments
      parse(args, res)
      res
    }

    def parse(args: Array[String], res: Arguments): Unit = {
      if (args != null) {
        var i = 0;
        while (i < args.length)
          if ((args(i) == null) || (args(i).length() == 0))
            i = i + 1
          else if (args(i).charAt(0) != optionPrefix) {
            res.addOther(args(i))
            i = i + 1
          } else if (options contains args(i)) {
            res.addOption(args(i))
            i = i + 1
          } else if (optionalArgs contains args(i)) {
            if ((i + 1) == args.length) {
              error("missing argument for '" + args(i) + "'")
              i = i + 1
            } else {
              res.addArgument(args(i), args(i + 1))
              i = i + 2
            }
          } else if (optionalBindings contains args(i)) {
            if ((i + 1) == args.length) {
              error("missing argument for '" + args(i) + "'")
              i = i + 1
            } else {
              res.addBinding(args(i),
                parseBinding(args(i + 1), optionalBindings(args(i))));
              i = i + 2
            }
          } else {
            var iter = prefixes.iterator
            val j = i
            while ((i == j) && iter.hasNext) {
              val prefix = iter.next
              if (args(i) startsWith prefix) {
                res.addPrefixed(prefix, args(i).substring(prefix.length()).trim());
                i = i + 1
              }
            }
            if (i == j) {
              val iter = prefixedBindings.keysIterator;
              while ((i == j) && iter.hasNext) {
                val prefix = iter.next
                if (args(i) startsWith prefix) {
                  val arg = args(i).substring(prefix.length()).trim()
                  i = i + 1
                  res.addBinding(prefix,
                    parseBinding(arg, prefixedBindings(prefix)));
                }
              }
              if (i == j) {
                error("unknown option '" + args(i) + "'")
                i = i + 1
              }
            }
          }
      }
    }
  }

  def parse(options: String*)(args: Array[String]): Arguments = {
    val parser = new Parser('-')
    val iter = options.iterator
    while (iter.hasNext)
      parser withOption iter.next
    parser.parse(args)
  }
}

class Arguments {

  private val options: Set[String] = new HashSet
  private val arguments: Map[String, String] = new HashMap
  private val prefixes: Map[String, Set[String]] = new HashMap
  private val bindings: Map[String, Map[String, String]] = new HashMap
  private val others: Buffer[String] = new ListBuffer

  def addOption(option: String): Unit = options += option

  def addArgument(option: String, arg: String): Unit = arguments(option) = arg

  def addPrefixed(prefix: String, arg: String): Unit =
    if (prefixes isDefinedAt prefix)
      prefixes(prefix) += arg
    else {
      prefixes(prefix) = new HashSet
      prefixes(prefix) += arg
    }

  def addBinding(tag: String, key: String, value: String): Unit =
    if (key.length() > 0) {
      if (bindings isDefinedAt tag)
        bindings(tag)(key) = value
      else {
        bindings(tag) = new HashMap
        bindings(tag)(key) = value
      }
    }

  def addBinding(tag: String, binding: Pair[String, String]): Unit =
    addBinding(tag, binding._1, binding._2)

  def addOther(arg: String): Unit = others += arg

  def contains(option: String): Boolean = options contains option

  def getArgument(option: String): Option[String] = arguments get option

  def getSuffixes(prefix: String): Set[String] =
    prefixes.get(prefix) match {
      case None => new HashSet
      case Some(set) => set
    }

  def containsSuffix(prefix: String, suffix: String): Boolean =
    prefixes.get(prefix) match {
      case None => false
      case Some(set) => set contains suffix
    }

  def getBindings(tag: String): Map[String, String] =
    bindings.get(tag) match {
      case None => new HashMap
      case Some(map) => map
    }

  def getBinding(option: String, key: String): Option[String] =
    bindings.get(option) match {
      case None => None
      case Some(map) => map get key
    }

  def getOthers: List[String] = others.toList

}
