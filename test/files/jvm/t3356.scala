import scala.actors.{Actor, Exit, !, UncaughtException}
import Actor._

case class ImageInfo(text: String) {
  def downloadImage(): ImageData = {
    ImageData(text)
  }
}

case class ImageData(text: String)
case class Download(info: ImageInfo)

object Test {

  def scanForImageInfo(url: String): List[ImageInfo] =
    List(ImageInfo("A"), ImageInfo("B"))

  def renderImage(data: ImageData) {
    println("rendering image "+data.text)
  }

  def renderImages(url: String) {
    val imageInfos = scanForImageInfo(url)
    println("sending download requests")
    val dataFutures = for (info <- imageInfos) yield {
      val loader = link {
        react { case Download(info) =>
          throw new Exception("no connection")
          reply(info.downloadImage())
        }; {}
      }
      loader !! Download(info)
    }
    var i = 0
    loopWhile (i < imageInfos.size) {
      i += 1
      val FutureInput = dataFutures(i-1).inputChannel
      react {
        case FutureInput ! (data @ ImageData(_)) =>
          renderImage(data)
        case Exit(from, ue: UncaughtException[_]) =>
          ue.message match {
            case Some(Download(info)) =>
              println("Couldn't download image "+info+" because of "+ue.getCause())
            case _ =>
              println("Couldn't download image because of "+ue.getCause())
          }
      }
    }
    println("OK, all images rendered.")
  }

  def main(args: Array[String]) {
    actor {
      renderImages("panorama.epfl.ch")
    }
  }

}
