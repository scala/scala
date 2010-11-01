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
        case Exit(from, UncaughtException(_, Some(Download(info)), _, _, cause)) =>
          println("Couldn't download image because of "+cause)
      }
    }
    println("OK, all images rendered.")
  }

  def main(args: Array[String]) {
    actor {
      self.trapExit = true
      renderImages("panorama.epfl.ch")
    }
  }

}
