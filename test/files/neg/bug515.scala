object Test extends App {
  class Truc {
    def getMachin() = "machin"
    def getMachinAsTruc() = this
  }
  val file = new Truc
  val parent: Truc = file.getMachin
}
