object Test {
  def main(args: Array[String]): Unit = {
    import java.lang.annotation.Retention;
    val c = classOf[Retention]
    val r: Retention = c.getAnnotation(c).asInstanceOf[Retention];
    Console.println(r.value)
  }
}
