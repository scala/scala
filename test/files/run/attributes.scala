object Test1 {
  class Foo {
    [remote]
    def foo: Unit = ()
  }
  def run: Unit = {
    val method = classOf[Foo].getMethod("foo", Array())
    method.getExceptionTypes foreach Console.println
  }
}

object Test2 {
  import java.io._
  class Reader(fname: String) {
    private val in = new BufferedReader(new FileReader(fname))
    [throws(classOf[IOException])]
    def read() = in.read()
  }
  def run: Unit = {
    val method = classOf[Reader].getMethod("read", Array())
    method.getExceptionTypes foreach Console.println
  }
}

/* Java:
public class Main {
    @Deprecated
    public void foo() {}
    public static void main(String[] args) throws Exception {
        Method method = Class.forName("test.Main").getMethod("foo", new Class[]{});
        Annotation annotation = method.getAnnotation(Deprecated.class);
        System.out.println(annotation); // @java.lang.Deprecated()
    }
}
*/
object Test3 {
  class Foo {
    [Deprecated]
    def foo: Unit = ()
  }
  def run: Unit = {
    val method = classOf[Foo].getMethod("foo", Array())
    val annotation = method.getAnnotation(classOf[Deprecated])
    Console.println(annotation)
  }
}

/* Java:
@Retention(value=RetentionPolicy.RUNTIME)
@interface Source {
   public String url();
   public String mail();
}
@Source(url="http://scala.epfl.ch", mail="scala@lists.epfl.ch")
class Foo {}
public class Main {
    public static void main(String[] args) throws Exception {
        Class clazz = Class.forName("test.Foo");
        Annotation[] annotations = clazz.getAnnotations();
        for (int i = 0; i < annotations.length; i++)
            System.out.println(annotations[i]);
        // @test.Main$Source(url=http://scala.epfl.ch, mail=scala@lists.epfl.ch)
    }
}
*/
object Test4 {
  import java.lang.annotation._
  [Retention(RetentionPolicy.RUNTIME)]
  class Source(url: String, mail: String) extends Attribute
  [Source("http://scala.epfl.ch", "scala@lists.epfl.ch")]
  class Foo
  def run: Unit = {
    val clazz = classOf[Foo]
    clazz.getAnnotations foreach Console.println
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    Test1.run
    Test2.run
    Test3.run
    Test4.run
  }
}
