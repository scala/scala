import java.io.File
import java.net.URLClassLoader

object StepTwo extends App {
  val classes = new File(System.getProperty("launch.step.three"))
  val cl = new URLClassLoader(Array(classes.toURI.toURL), getClass.getClassLoader)
  val stepThree = cl.loadClass("StepThree")
  val main = stepThree.getDeclaredMethod("main", classOf[Array[String]])
  main.invoke(null, Array[String]())
}