import scala.actors.Futures._

object Test {
  def main(args: Array[String]) {
    val x = future {
      System.out.println(1)
      future {
        System.out.println(2)
        future {
          System.out.println(3)
          future {
            System.out.println(4)
            future {
              System.out.println(5)
            }()
          }()
        }()
      }()
    }()
  }
}
