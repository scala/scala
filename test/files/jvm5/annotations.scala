object Test1 {
  class Foo {
    @remote
    def foo: Unit = ()
  }
  def run {
    val method = classOf[Foo].getMethod("foo", Array())
    method.getExceptionTypes foreach println
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
    method.getExceptionTypes foreach println
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
  import java.lang.Deprecated
  class Foo {
    @Deprecated
    def foo: Unit = ()
  }
  def run {
    val method = classOf[Foo].getMethod("foo", Array())
    val annotation = method.getAnnotation(classOf[Deprecated])
    println(annotation)
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
        // @test.Main$Source(url=http://scala-lang.org, mail=scala@lists.epfl.ch)
    }
}
*/
object Test4 {
  import test.SourceAnnotation // defined in SourceAnnotation.java
  @SourceAnnotation{val value = "http://scala-lang.org",
                    val mails = Array("scala@lists.epfl.ch", "scala-lounge@lists.epfl.ch")}
  class Foo1
  @SourceAnnotation("http://bloodsuckers.com") { val mails = Array("you@bloodsuckers.com") }
  class Foo2
  @SourceAnnotation("http://bloodsuckers.com")
  class Foo3
  class Foo4 {
    @SourceAnnotation("file:///dev/null")
    val x = 1
  }
  class Foo5 {
    @SourceAnnotation("file:///dev/zero")
    def bar: Int = 0
  }
  class Foo6 @SourceAnnotation("primary constructor")(s: String) {
    // to guarantee that primary constructor annotations
    // are not applied to secondary constructors
    def this() = this("")
  }
  class Foo7(s: String) {
    @SourceAnnotation("secondary constructor")
    def this() = this("")
  }
  class Foo8(@SourceAnnotation("constructor val") val n: Int) {}
  def run {
    import java.lang.annotation.Annotation
    import java.lang.reflect.AnnotatedElement
    def printSourceAnnotations(target: AnnotatedElement) {
      //print SourceAnnotation in a predefined way to insure
      // against difference in the JVMs (e.g. Sun's vs IBM's)
      def printSourceAnnotation(a: Annotation) {
        val ann = a.asInstanceOf[SourceAnnotation]
        println("@test.SourceAnnotation(mails=" + ann.mails.deepMkString("{", ",", "}") +
                ", value=" + ann.value + ")")
      }
      val anns = target.getAnnotations()
      anns foreach printSourceAnnotation
      if (anns.length > 0) {
        println(target)
        println
      }
    }
    printSourceAnnotations(classOf[Foo1])
    printSourceAnnotations(classOf[Foo2])
    printSourceAnnotations(classOf[Foo3])
    classOf[Foo4].getDeclaredFields  foreach printSourceAnnotations
    classOf[Foo4].getDeclaredMethods foreach printSourceAnnotations
    classOf[Foo5].getDeclaredMethods foreach printSourceAnnotations
    classOf[Foo6].getDeclaredConstructors foreach printSourceAnnotations
    classOf[Foo7].getDeclaredConstructors foreach printSourceAnnotations
    classOf[Foo8].getDeclaredFields  foreach printSourceAnnotations
    classOf[Foo8].getDeclaredMethods foreach printSourceAnnotations
  }
}

object Test5 {
  import scala.reflect.BeanProperty
  import java.lang.Integer

  class Count {
    // we use "Integer" instead of "Int" because of Java reflection
    @BeanProperty
    private var count: Integer = 0

    private val getter =
      getClass().getMethod("getCount", Array[java.lang.Class[T] forSome { type T }]())
    private val setter =
      getClass().getMethod("setCount", Array(classOf[Integer]))

    def get = getter.invoke(this, Array()).asInstanceOf[Integer].intValue
    def set(n: Int) = setter.invoke(this, Array(new Integer(n)))
  }
  def run {
    val count = new Count
    println(count.get)
    count.set(99)
    println(count.get)
  }
}

object Test {
  def main(args: Array[String]) {
    Test1.run
    Test2.run
    Test3.run     // requires the use of -target:jvm-1.5
    Test4.run
    Test5.run
  }
}
