// a.scala
// Mon Jul 11 14:18:26 PDT 2011

object ExistentialsConsideredHarmful {
  class Animal(val name: String)
  object Dog extends Animal("Dog")
  object Sheep extends Animal("Sheep")

  trait Tools[A] {
    def shave(a: A): A
  }
  def tools[A](a: A): Tools[A] = null // dummy

  case class TransportBox[A <: Animal](animal: A, tools: Tools[A]) {
    def label: String = animal.name
  }

  // 1.
  def carry[A <: Animal](box: TransportBox[A]): Unit = {
    println(box.animal.name+" got carried away")
  }

  val aBox =
    if (math.random < 0.5)
      TransportBox(Dog, tools(Dog))
    else
      TransportBox(Sheep, tools(Sheep))

  // 2.
  //aBox.tools.shave(aBox.animal)

  // Use pattern match to avoid opening the existential twice
  aBox match {
    case TransportBox(animal, tools) => tools.shave(animal)
  }

  abstract class BoxCarrier[R <: Animal](box: TransportBox[R]) {
    def speed: Int

    def talkToAnimal: Unit = println("The carrier says hello to"+box.animal.name)
  }

  // 3.
  //val bc = new BoxCarrier(aBox) {

  // Use pattern match to avoid opening the existential twice
  // Type annotation on bc is required ... possible compiler bug?
  // val bc : BoxCarrier[_ <: Animal] = aBox match {
  val bc = aBox match {
    case tb : TransportBox[a] => new BoxCarrier(tb) {
      def speed: Int = 12
    }
  }
}
// existentials-harmful.scala:50:
//     case tb : TransportBox[a] => new BoxCarrier(tb) {
//               ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  ExistentialsConsideredHarmful.TransportBox[_1]
//         pt  ExistentialsConsideredHarmful.TransportBox[_1]
//      pattp  ExistentialsConsideredHarmful.TransportBox[a]
//   pattp+pt  ExistentialsConsideredHarmful.TransportBox[a]
//   pt+pattp  ExistentialsConsideredHarmful.TransportBox[_1]
//     result  ExistentialsConsideredHarmful.TransportBox[_1]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }