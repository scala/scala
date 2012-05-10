import sbt._
import Keys._

object Release {

  // TODO - Just make the STARR artifacts and dump the sha1 files.


  lazy val pushStarr = Command.command("push-starr") { (state: State) =>
      // TODO do something
      // Revert to previous project state.
      state
   }

}
