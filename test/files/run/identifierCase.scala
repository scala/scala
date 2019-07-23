object Test {

  val $reserved      = "$reserved"
  val ǅul            = "ǅul"
  val ǂnûm           = "ǂnûm"
  val ⅰ_ⅲ            = "ⅰ_ⅲ"
  val Ⅰ_Ⅲ            = "Ⅰ_Ⅲ"
  val ↁelerious      = "ↁelerious"
  val ǃqhàà          = "ǃqhàà"
  val ʹthatsaletter  = "ʹthatsaletter"

  def main(args: Array[String]): Unit = {
    val s = "foo" match {
      case $reserved     => "constant"
      case ǅul           => "constant"
      case ǂnûm          => "constant"
      case ⅰ_ⅲ           => "constant"
      case Ⅰ_Ⅲ           => "constant"
      case ↁelerious     => "constant"
      case ǃqhàà         => "constant"
      case ʹthatsaletter => "constant"
      case ªpple         => "not so constant"
      case ʰelper        => "unreachable"
    }
    println(s)
  }

  //all non-op characters can follow a "normal" leading letter
  //if any of the second characters below weren't letters or numbers, this wouldn't compile
  //they are not numbers since they are used as leading characters above

  val a$reserved      = "a$"
  val aǅul            = "aǅul"
  val aǂnûm           = "aǂnûm"
  val aⅰ_ⅲ            = "aⅰ_ⅲ"
  val aⅠ_Ⅲ            = "aⅠ_Ⅲ"
  val aↁelerious      = "aↁelerious"
  val aǃqhàà          = "aǃqhàà"
  val aʹthatsaletter  = "aʹthatsaletter"
  val anªpple         = "anªpple"
  val aʰelper         = "aʰelper"
}