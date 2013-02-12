package scala
package io

trait AnsiColor {
  /** Foreground color for ANSI black */
  final val BLACK      = "\033[30m"
  /** Foreground color for ANSI red */
  final val RED        = "\033[31m"
  /** Foreground color for ANSI green */
  final val GREEN      = "\033[32m"
  /** Foreground color for ANSI yellow */
  final val YELLOW     = "\033[33m"
  /** Foreground color for ANSI blue */
  final val BLUE       = "\033[34m"
  /** Foreground color for ANSI magenta */
  final val MAGENTA    = "\033[35m"
  /** Foreground color for ANSI cyan */
  final val CYAN       = "\033[36m"
  /** Foreground color for ANSI white */
  final val WHITE      = "\033[37m"

  /** Background color for ANSI black */
  final val BLACK_B    = "\033[40m"
  /** Background color for ANSI red */
  final val RED_B      = "\033[41m"
  /** Background color for ANSI green */
  final val GREEN_B    = "\033[42m"
  /** Background color for ANSI yellow */
  final val YELLOW_B   = "\033[43m"
  /** Background color for ANSI blue */
  final val BLUE_B     = "\033[44m"
  /** Background color for ANSI magenta */
  final val MAGENTA_B  = "\033[45m"
  /** Background color for ANSI cyan */
  final val CYAN_B     = "\033[46m"
  /** Background color for ANSI white */
  final val WHITE_B    = "\033[47m"

  /** Reset ANSI styles */
  final val RESET      = "\033[0m"
  /** ANSI bold */
  final val BOLD       = "\033[1m"
  /** ANSI underlines */
  final val UNDERLINED = "\033[4m"
  /** ANSI blink */
  final val BLINK      = "\033[5m"
  /** ANSI reversed */
  final val REVERSED   = "\033[7m"
  /** ANSI invisible */
  final val INVISIBLE  = "\033[8m"
}

object AnsiColor extends AnsiColor { }
