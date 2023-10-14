object Test {
  def isAdmin(accessLevel: String): Boolean =
    accessLevel != "user‮ ⁦// Check if admin⁩ ⁦"

  def שרה = 0 // no bidi override char, these characters are rtl

  cl‮ass C

  def a‮cb

  // comm‮tne

  // comment with direction indicator that isn't a threat:
  //  - From the Latin "coævus": com- ‎("equal") in combination with aevum ‎(aevum, "age").

  """te‮tx"""
  raw"""te‮tx"""

  val u202e = '‮'

  def main(args: Array[String]): Unit = {
    println(isAdmin("user"))
  }
}
