import scala.collection.mutable

/** All contexts where objects can be embedded. */
object Contexts extends Enumeration {
  val Class, Object, Trait, Method, PrivateMethod, Anonfun, ClassConstructor, TraitConstructor, LazyVal, Val = Value

  val topLevel = List(Class, Object, Trait)
}


/** Test generation of inner objects, trying to cover as many cases as possible. It proceeds
 *  by progressively adding nesting layers around a 'payload body'.
 *
 *  There are three scenarios (each generating a full combinatorial search):
 *   - plain object with single-threaded access
 *   - private object with single-threaded access
 *   - plain object with multi-threaded access.
 *
 *  Special care is taken to skip problematic cases (or known bugs). For instance,
 *  it won't generate objects inside lazy vals (leads to deadlock), or objects that
 *  are initialized in the static constructors (meaning inside 'val' inside a top-level
 *  object, or equivalent).
 *
 *  Usage: TestGen <nr of levels>
 *     - by default it's 2 levels. Currently, 3-level deep uncovers bugs in the type checker.
 *
 * @author Iulian Dragos
 */
object TestGen {
  val testFile = "object-testers-automated.scala"

  val payload =
"""      var ObjCounter = 0

      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }
"""

  val payloadPrivate =
"""      var ObjCounter = 0

      private object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }
"""

  val payloadMT =
"""     @volatile var ObjCounter = 0

      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
       val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
           def run = Obj
         })

       threads foreach (_.start())
       threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }
"""


  import Contexts._

  val template =
"""
%s

%s

object Test {
  def main(args: Array[String]) {
    %s
  }
}
"""

  var counter = 0
  def freshName(name: String) = {
    counter += 1
    name + counter
  }

  val bodies = new mutable.ListBuffer[String]
  val triggers = new mutable.ListBuffer[String]

  /** Generate the nesting code. */
  def generate(depth: Int,    // how many levels we still need to 'add' around the current body
               body: String,  // the body of one test, so far
               trigger: String,   // the code that needs to be invoked to run the test so far
               nested: List[Contexts.Value],  // the path from the innermost to the outermost context
               p: List[Contexts.Value] => Boolean,  // a predicate for filtering problematic cases
               privateObj: Boolean = false) {  // are we using a private object?

    def shouldBeTopLevel =
      ((depth == 1)
       || (nested.headOption == Some(PrivateMethod))
       || (nested.isEmpty && privateObj))

    val enums =
      if (shouldBeTopLevel) Contexts.topLevel else Contexts.values.toList

    if (depth == 0) {
      if (p(nested)) {bodies += body; triggers += trigger }
    } else {
      for (ctx <- enums) {
        val (body1, trigger1) = ctx match {
         case Class =>
           val name = freshName("Class") + "_" + depth
           ("""
             class %s {
               %s
               def run { %s }
             }
           """.format(name, body, trigger), "(new %s).run".format(name))

         case Trait =>
           val name = freshName("Trait") + "_" + depth
           ("""
             trait %s {
               %s
               def run { %s }
             }
           """.format(name, body, trigger), "(new %s {}).run".format(name))

         case Object =>
           val name = freshName("Object") + "_" + depth
           ("""
             object %s {
               %s
               def run { %s } // trigger
             }
           """.format(name, body, trigger), "%s.run".format(name))

         case Method =>
           val name = freshName("method") + "_" + depth
           ("""
             def %s {
               %s
               %s // trigger
             }
           """.format(name, body, trigger), name)

         case PrivateMethod =>
           val name = freshName("method") + "_" + depth
           ("""
             private def %s {
               %s
               %s // trigger
             }
           """.format(name, body, trigger), name)

          case Val =>
            val name = freshName("value") + "_" + depth
            ("""
               val %s = {
                 %s
                 %s // trigger
               }
             """.format(name, body, trigger), name)

          case LazyVal =>
            val name = freshName("lzvalue") + "_" + depth
            ("""
               lazy val %s = {
                 %s
                 %s // trigger
               }
             """.format(name, body, trigger), name)

          case Anonfun =>
            val name = freshName("fun") + "_" + depth
            ("""
               val %s = () => {
                 %s
                 %s // trigger
               }
             """.format(name, body, trigger), name + "()")

          case ClassConstructor =>
           val name = freshName("Class") + "_" + depth
           ("""
             class %s {
               { // in primary constructor
                 %s
                 %s // trigger
               }
             }
           """.format(name, body, trigger), "(new %s)".format(name))

          case TraitConstructor =>
           val name = freshName("Trait") + "_" + depth
           ("""
             trait %s {
               { // in primary constructor
                 %s
                 %s // trigger
               }
             }
           """.format(name, body, trigger), "(new %s {})".format(name))

        }
        generate(depth - 1, body1, trigger1, ctx :: nested, p)
      }
    }
  }

  /** Only allow multithreaded tests if not inside a static initializer. */
  private def allowMT(structure: List[Contexts.Value]): Boolean = {
    var nesting = structure
    while ((nesting ne Nil) && nesting.head == Object) {
      nesting = nesting.tail
    }
    if (nesting ne Nil)
      !(nesting.head == Val)
    else
      true
  } && !objectInsideLazyVal(structure)

  /** Known bug: object inside lazyval leads to deadlock. */
  private def objectInsideLazyVal(structure: List[Contexts.Value]): Boolean =
    structure.contains(LazyVal)


  def usage() {
    val help =
"""
  Usage: TestGen <nr of levels>

  <nr of levels> - how deeply nested should the objects be? default is 2.
                   (Currently, 3-level deep uncovers bugs in the type checker).

 Test generation of inner objects, trying to cover as many cases as possible. It proceeds
  by progressively adding nesting layers around a 'payload body'.

  There are three scenarios (each generating a full combinatorial search):
   - plain object with single-threaded access
   - private object with single-threaded access
   - plain object with multi-threaded access.

  Special care is taken to skip problematic cases (or known bugs). For instance,
  it won't generate objects inside lazy vals (leads to deadlock), or objects that
  are initialized in the static constructors (meaning inside 'val' inside a top-level
  object, or equivalent).
"""

    println(help)
    System.exit(1)
  }

  def main(args: Array[String]) {
    if (args.isEmpty || args.contains("-help")) usage()

    val depth = if (args.length < 1) 2 else args(0).toInt

    val header =
"""
/* ================================================================================
         Automatically generated on %tF. Do Not Edit (unless you have to).
         (%d-level nesting)
   ================================================================================ */
""".format(new java.util.Date, depth)

    generate(depth, payload, "runTest", List(), x => true)
    // private
    generate(depth, payloadPrivate, "runTest", List(), x => true, true)
    generate(depth, payloadMT, "runTest", List(), allowMT)

    println(template.format(header, bodies.mkString("", "\n", ""), triggers.mkString("", "\n", "")))
  }
}
