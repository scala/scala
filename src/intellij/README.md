Use the latest IntelliJ IDEA release and install the Scala plugin from within the IDE.

Compilation withing IDEA is performed in "-Dlocker.skip=1" mode: the sources are built
directly using the STARR compiler.

The following steps are required to use IntelliJ IDEA on Scala trunk
 - Run `ant init`. This will download some JARs to `./build/deps`, which are included in IntelliJ's classpath.
 - Run `./src/intellij/setup.sh`.
 - Open `./src/intellij/scala.ipr` in IntelliJ.
 - `File` → `Project Structure` → `Project` → `Project SDK`. Create an SDK entry named "1.6" containing the Java 1.6 SDK. (Or other SDK version; see "Requirements" in the repo's main README.)

Compilation within IDEA is performed in `-Dlocker.skip=1` mode: the sources are built
directly using the STARR compiler (which is downloaded from [the Central Repository](http://central.sonatype.org/), according to `starr.version` in `versions.properties`).
