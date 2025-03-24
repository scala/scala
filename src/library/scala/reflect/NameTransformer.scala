/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect

/** Provides functions to encode and decode Scala symbolic names.
 *  Also provides some constants.
 */
object NameTransformer {
  // TODO: reduce duplication with and in StdNames
  // I made these constants because we cannot change them without bumping our major version anyway.
  final val NAME_JOIN_STRING              = "$"
  final val MODULE_SUFFIX_STRING          = "$"
  final val MODULE_INSTANCE_NAME          = "MODULE$"
  final val LOCAL_SUFFIX_STRING           = " "
  final val LAZY_LOCAL_SUFFIX_STRING      = "$lzy"
  final val MODULE_VAR_SUFFIX_STRING      = "$module"
  final val SETTER_SUFFIX_STRING          = "_$eq"
  final val TRAIT_SETTER_SEPARATOR_STRING = "$_setter_$"

  private[this] val nops = 128
  private[this] val ncodes = 26 * 26

  private class OpCodes(val op: Char, val code: String, val next: OpCodes)

  private[this] val op2code = new Array[String](nops)
  private[this] val code2op = new Array[OpCodes](ncodes)
  private def enterOp(op: Char, code: String) = {
    op2code(op.toInt) = code
    val c = (code.charAt(1) - 'a') * 26 + code.charAt(2) - 'a'
    code2op(c.toInt) = new OpCodes(op, code, code2op(c))
  }

  /* Note: decoding assumes opcodes are only ever lowercase. */
  enterOp('~', "$tilde")
  enterOp('=', "$eq")
  enterOp('<', "$less")
  enterOp('>', "$greater")
  enterOp('!', "$bang")
  enterOp('#', "$hash")
  enterOp('%', "$percent")
  enterOp('^', "$up")
  enterOp('&', "$amp")
  enterOp('|', "$bar")
  enterOp('*', "$times")
  enterOp('/', "$div")
  enterOp('+', "$plus")
  enterOp('-', "$minus")
  enterOp(':', "$colon")
  enterOp('\\', "$bslash")
  enterOp('?', "$qmark")
  enterOp('@', "$at")

  /** Replace operator symbols by corresponding `\$opname`.
   *
   *  @param name the string to encode
   *  @return     the string with all recognized opchars replaced with their encoding
   */
  def encode(name: String): String = {
    var buf: StringBuilder = null
    val len = name.length()
    var i = 0
    while (i < len) {
      val c = name charAt i
      if (c < nops && (op2code(c.toInt) ne null)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.substring(0, i))
        }
        buf.append(op2code(c.toInt))
      /* Handle glyphs that are not valid Java/JVM identifiers */
      }
      else if (!Character.isJavaIdentifierPart(c)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.substring(0, i))
        }
        buf.append("$u%04X".format(c.toInt))
      }
      else if (buf ne null) {
        buf.append(c)
      }
      i += 1
    }
    if (buf eq null) name else buf.toString()
  }

  /** Replace `\$opname` by corresponding operator symbol.
   *
   *  @param name0 the string to decode
   *  @return      the string with all recognized operator symbol encodings replaced with their name
   */
  def decode(name0: String): String = {
    //System.out.println("decode: " + name);//DEBUG
    val name = if (name0.endsWith("<init>")) name0.stripSuffix("<init>") + "this"
               else name0
    var buf: StringBuilder = null
    val len = name.length()
    var i = 0
    while (i < len) {
      var ops: OpCodes = null
      var unicode = false
      val c = name charAt i
      if (c == '$' && i + 2 < len) {
        val ch1 = name.charAt(i+1)
        if ('a' <= ch1 && ch1 <= 'z') {
          val ch2 = name.charAt(i+2)
          if ('a' <= ch2 && ch2 <= 'z') {
            ops = code2op((ch1 - 'a') * 26 + ch2 - 'a')
            while ((ops ne null) && !name.startsWith(ops.code, i)) ops = ops.next
            if (ops ne null) {
              if (buf eq null) {
                buf = new StringBuilder()
                buf.append(name.substring(0, i))
              }
              buf.append(ops.op)
              i += ops.code.length()
            }
            /* Handle the decoding of Unicode glyphs that are
             * not valid Java/JVM identifiers */
          } else if ((len - i) >= 6 && // Check that there are enough characters left
                     ch1 == 'u' &&
                     ((Character.isDigit(ch2)) ||
                     ('A' <= ch2 && ch2 <= 'F'))) {
            /* Skip past "$u", next four should be hexadecimal */
            val hex = name.substring(i+2, i+6)
            try {
              val str = Integer.parseInt(hex, 16).toChar
              if (buf eq null) {
                buf = new StringBuilder()
                buf.append(name.substring(0, i))
              }
              buf.append(str)
              /* 2 for "$u", 4 for hexadecimal number */
              i += 6
              unicode = true
            } catch {
              case _:NumberFormatException =>
                /* `hex` did not decode to a hexadecimal number, so
                 * do nothing. */
            }
          }
        }
      }
      /* If we didn't see an opcode or encoded Unicode glyph, and the
        buffer is non-empty, write the current character and advance
         one */
      if ((ops eq null) && !unicode) {
        if (buf ne null)
          buf.append(c)
        i += 1
      }
    }
    //System.out.println("= " + (if (buf == null) name else buf.toString()));//DEBUG
    if (buf eq null) name else buf.toString()
  }
}
