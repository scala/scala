/**
 * @author Vlad Ureche
 */
package scala.tools.nsc.doc
package html.page.diagram

object DiagramStats {

  class TimeTracker(title: String) {
    var totalTime: Long = 0l
    var maxTime: Long = 0l
    var instances: Int = 0

    def addTime(ms: Long) = {
      if (maxTime < ms)
        maxTime = ms
      totalTime += ms
      instances += 1
    }

    def printStats(print: String => Unit) = {
      if (instances == 0)
        print(title + ": no stats gathered")
      else {
        print("  " + title)
        print("  " + "=" * title.length)
        print("    count:        " + instances + " items")
        print("    total time:   " + totalTime + " ms")
        print("    average time: " + (totalTime/instances) + " ms")
        print("    maximum time: " + maxTime + " ms")
        print("")
      }
    }
  }

  private[this] val filterTrack = new TimeTracker("diagrams model filtering")
  private[this] val modelTrack = new TimeTracker("diagrams model generation")
  private[this] val dotGenTrack = new TimeTracker("dot diagram generation")
  private[this] val dotRunTrack = new TimeTracker("dot process running")
  private[this] val svgTrack = new TimeTracker("svg processing")
  private[this] var brokenImages = 0
  private[this] var fixedImages = 0

  def printStats(settings: Settings) = {
    if (settings.docDiagramsDebug) {
      settings.printMsg("\nDiagram generation running time breakdown:\n")
      filterTrack.printStats(settings.printMsg)
      modelTrack.printStats(settings.printMsg)
      dotGenTrack.printStats(settings.printMsg)
      dotRunTrack.printStats(settings.printMsg)
      svgTrack.printStats(settings.printMsg)
      println("  Broken images: " + brokenImages)
      println("  Fixed images: " + fixedImages)
      println("")
    }
  }

  def addFilterTime(ms: Long) = filterTrack.addTime(ms)
  def addModelTime(ms: Long) = modelTrack.addTime(ms)
  def addDotGenerationTime(ms: Long) = dotGenTrack.addTime(ms)
  def addDotRunningTime(ms: Long) = dotRunTrack.addTime(ms)
  def addSvgTime(ms: Long) = svgTrack.addTime(ms)

  def addBrokenImage(): Unit = brokenImages += 1
  def addFixedImage(): Unit = fixedImages += 1
}
