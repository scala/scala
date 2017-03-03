package scala
package io

/** ANSI escape codes providing control over text formatting and color on supporting text terminals.
  *
  * ==ANSI Style and Control Codes==
  *
  * This group of escape codes provides control over text styling. For example, to turn on reverse video with bold and
  * then turn off all styling embed these codes,
  *
  * {{{
  * import io.AnsiColor._
  *
  * object ColorDemo extends App {
  *
  *   println(s"${REVERSED}${BOLD}Hello 1979!${RESET}")
  * }
  * }}}
  *
  * ==Foreground and Background Colors==
  *
  * Embedding ANSI color codes in text output will control the text foreground and background colors.
  *
  * <table>
  *   <tr><th style="padding:4px 15px;text-decoration:underline">Foreground</th><th style="width:50%"></th><th style="padding:4px 15px;text-decoration:underline">Background</th></tr>
  *   <tr><td style="padding:4px 15px">BLACK  </td><td style="background-color:#000"></td><td style="padding:4px 15px">BLACK_B  </td></tr>
  *   <tr><td style="padding:4px 15px">RED    </td><td style="background-color:#f00"></td><td style="padding:4px 15px">RED_B    </td></tr>
  *   <tr><td style="padding:4px 15px">GREEN  </td><td style="background-color:#0f0"></td><td style="padding:4px 15px">GREEN_B  </td></tr>
  *   <tr><td style="padding:4px 15px">YELLOW </td><td style="background-color:#ff0"></td><td style="padding:4px 15px">YELLOW_B </td></tr>
  *   <tr><td style="padding:4px 15px">BLUE   </td><td style="background-color:#00f"></td><td style="padding:4px 15px">BLUE_B   </td></tr>
  *   <tr><td style="padding:4px 15px">MAGENTA</td><td style="background-color:#f0f"></td><td style="padding:4px 15px">MAGENTA_B</td></tr>
  *   <tr><td style="padding:4px 15px">CYAN   </td><td style="background-color:#0ff"></td><td style="padding:4px 15px">CYAN_B   </td></tr>
  *   <tr><td style="padding:4px 15px">WHITE  </td><td style="background-color:#fff"></td><td style="padding:4px 15px">WHITE_B  </td></tr>
  * </table>
  *
  * @groupname style-control ANSI Style and Control Codes
  * @groupprio style-control 101
  *
  * @groupname color-black ANSI Black
  * @groupdesc color-black <table style="width:100%"><tr><td style="background-color:#000">&nbsp;</td></tr></table>
  * @groupprio color-black 110
  *
  * @groupname color-red ANSI Red
  * @groupdesc color-red <table style="width:100%"><tr><td style="background-color:#f00">&nbsp;</td></tr></table>
  * @groupprio color-red 120
  *
  * @groupname color-green ANSI Green
  * @groupdesc color-green <table style="width:100%"><tr><td style="background-color:#0f0">&nbsp;</td></tr></table>
  * @groupprio color-green 130
  *
  * @groupname color-yellow ANSI Yellow
  * @groupdesc color-yellow <table style="width:100%"><tr><td style="background-color:#ff0">&nbsp;</td></tr></table>
  * @groupprio color-yellow 140
  *
  * @groupname color-blue ANSI Blue
  * @groupdesc color-blue <table style="width:100%"><tr><td style="background-color:#00f">&nbsp;</td></tr></table>
  * @groupprio color-blue 150
  *
  * @groupname color-magenta ANSI Magenta
  * @groupdesc color-magenta <table style="width:100%"><tr><td style="background-color:#f0f">&nbsp;</td></tr></table>
  * @groupprio color-magenta 160
  *
  * @groupname color-cyan ANSI Cyan
  * @groupdesc color-cyan <table style="width:100%"><tr><td style="background-color:#0ff">&nbsp;</td></tr></table>
  * @groupprio color-cyan 170
  *
  * @groupname color-white ANSI White
  * @groupdesc color-white <table style="width:100%"><tr><td style="background-color:#fff">&nbsp;</td></tr></table>
  * @groupprio color-white 180
  */
trait AnsiColor {
  /** Foreground color for ANSI black
    * @group color-black
    */
  final val BLACK      = "\u001b[30m"
  /** Foreground color for ANSI red
    * @group color-red
    */
  final val RED        = "\u001b[31m"
  /** Foreground color for ANSI green
    * @group color-green
    */
  final val GREEN      = "\u001b[32m"
  /** Foreground color for ANSI yellow
    * @group color-yellow
    */
  final val YELLOW     = "\u001b[33m"
  /** Foreground color for ANSI blue
    * @group color-blue
    */
  final val BLUE       = "\u001b[34m"
  /** Foreground color for ANSI magenta
    * @group color-magenta
    */
  final val MAGENTA    = "\u001b[35m"
  /** Foreground color for ANSI cyan
    * @group color-cyan
    */
  final val CYAN       = "\u001b[36m"
  /** Foreground color for ANSI white
    * @group color-white
    */
  final val WHITE      = "\u001b[37m"

  /** Background color for ANSI black
    * @group color-black
    */
  final val BLACK_B    = "\u001b[40m"
  /** Background color for ANSI red
    * @group color-red
    */
  final val RED_B      = "\u001b[41m"
  /** Background color for ANSI green
    * @group color-green
    */
  final val GREEN_B    = "\u001b[42m"
  /** Background color for ANSI yellow
    * @group color-yellow
    */
  final val YELLOW_B   = "\u001b[43m"
  /** Background color for ANSI blue
    * @group color-blue
    */
  final val BLUE_B     = "\u001b[44m"
  /** Background color for ANSI magenta
    * @group color-magenta
    */
  final val MAGENTA_B  = "\u001b[45m"
  /** Background color for ANSI cyan
    * @group color-cyan
    */
  final val CYAN_B     = "\u001b[46m"
  /** Background color for ANSI white
    * @group color-white
    */
  final val WHITE_B    = "\u001b[47m"

  /** Reset ANSI styles
    * @group style-control
    */
  final val RESET      = "\u001b[0m"
  /** ANSI bold
    * @group style-control
    */
  final val BOLD       = "\u001b[1m"
  /** ANSI underlines
    * @group style-control
    */
  final val UNDERLINED = "\u001b[4m"
  /** ANSI blink
    * @group style-control
    */
  final val BLINK      = "\u001b[5m"
  /** ANSI reversed
    * @group style-control
    */
  final val REVERSED   = "\u001b[7m"
  /** ANSI invisible
    * @group style-control
    */
  final val INVISIBLE  = "\u001b[8m"
}

object AnsiColor extends AnsiColor { }
