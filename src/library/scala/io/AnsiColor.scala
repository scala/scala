package scala
package io

trait AnsiColor {
  /** Foreground color for ANSI black */
  final val BLACK      = "\u001b[30m"
  /** Foreground color for ANSI red */
  final val RED        = "\u001b[31m"
  /** Foreground color for ANSI green */
  final val GREEN      = "\u001b[32m"
  /** Foreground color for ANSI yellow */
  final val YELLOW     = "\u001b[33m"
  /** Foreground color for ANSI blue */
  final val BLUE       = "\u001b[34m"
  /** Foreground color for ANSI magenta */
  final val MAGENTA    = "\u001b[35m"
  /** Foreground color for ANSI cyan */
  final val CYAN       = "\u001b[36m"
  /** Foreground color for ANSI white */
  final val WHITE      = "\u001b[37m"

  /** Background color for ANSI black */
  final val BLACK_B    = "\u001b[40m"
  /** Background color for ANSI red */
  final val RED_B      = "\u001b[41m"
  /** Background color for ANSI green */
  final val GREEN_B    = "\u001b[42m"
  /** Background color for ANSI yellow */
  final val YELLOW_B   = "\u001b[43m"
  /** Background color for ANSI blue */
  final val BLUE_B     = "\u001b[44m"
  /** Background color for ANSI magenta */
  final val MAGENTA_B  = "\u001b[45m"
  /** Background color for ANSI cyan */
  final val CYAN_B     = "\u001b[46m"
  /** Background color for ANSI white */
  final val WHITE_B    = "\u001b[47m"

  /** Reset ANSI styles */
  final val RESET      = "\u001b[0m"
  /** ANSI bold */
  final val BOLD       = "\u001b[1m"
  /** ANSI underlines */
  final val UNDERLINED = "\u001b[4m"
  /** ANSI blink */
  final val BLINK      = "\u001b[5m"
  /** ANSI reversed */
  final val REVERSED   = "\u001b[7m"
  /** ANSI invisible */
  final val INVISIBLE  = "\u001b[8m"
}

object AnsiColor extends AnsiColor { }
