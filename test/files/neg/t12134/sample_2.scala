//> using options -Xplugin:. -Xplugin-require:unplugged -d testy.jar
package sample

// The unplugged plugin pre-emptively creates a read-only testy.jar.
// Previously, compilation would NPE after failing to write the output.
// Now the error is terse but accurate.
// The plugin updates -d to put the file under partest output,
// which happens to be the same as -Xplugin.
object Main extends App
