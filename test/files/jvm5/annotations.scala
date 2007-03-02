import java.lang.Deprecated

object Test1 {
  class Foo {
    @remote
    def foo: Unit = ()
  }
  def run {
    val method = classOf[Foo].getMethod("foo", Array())
    method.getExceptionTypes foreach Console.println
  }
}

object Test2 {
  import java.io.{BufferedReader,FileReader, IOException}
  class Reader(fname: String) {
    private val in = new BufferedReader(new FileReader(fname))

    @throws(classOf[IOException])
    def read() = in.read()
  }
  def run {
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
    @Deprecated
    def foo: Unit = ()
  }
  def run {
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
  import test.SourceAnnotation // defined in SourceAnnotation.java
  @SourceAnnotation{val value = "http://scala.epfl.ch", val mail = "scala@lists.epfl.ch"}
  class Foo1
  @SourceAnnotation("http://bloodsuckers.com") { val mail = "you@bloodsuckers.com" }
  class Foo2
  @SourceAnnotation("http://bloodsuckers.com")
  class Foo3
  def run {
    classOf[Foo1].getAnnotations foreach Console.println
    classOf[Foo2].getAnnotations foreach Console.println
    classOf[Foo3].getAnnotations foreach Console.println
  }
}

object Test5 {
  import scala.reflect.BeanProperty
  class Count {
    // we use "Integer" instead of "Int" because of Java reflection
    @BeanProperty
    private var count: Integer = 0

    private val getter =
      getClass().getMethod("getCount", Array[java.lang.Class]())
    private val setter =
      getClass().getMethod("setCount", Array[java.lang.Class](classOf[Integer]))

    def get = getter.invoke(this, Array()).asInstanceOf[Integer].intValue
    def set(n: Int) = setter.invoke(this, Array(new Integer(n)))
  }
  def run {
    val count = new Count
    Console.println(count.get)
    count.set(99)
    Console.println(count.get)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    Test1.run
    Test2.run
    Test3.run     // requires the use of -target:jvm-1.5
    Test4.run
    Test5.run
  }
}
