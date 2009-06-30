/* https://lampsvn.epfl.ch/trac/scala/ticket/2104
   symptom: Source via Buffered Source always loses the last char of the input file.
   cause: BufferedSource? doesn't check return for -1 (EOF), and uses reader.ready() improperly as a substitute.

  test: check over all possible strings of length up to N over alphabet chars:
  write file, then read back its chars, and get back the original.

*/
object Test
{
  val N=4

  import java.io.{ File => JFile }
  import java.io.FileWriter
  import io.Source
  def overwrite(file: JFile,w: FileWriter=>Unit) {
    val fw=new FileWriter(file)
    w(fw)
    fw.close
  }
  def delete_after(f: JFile,g: Source=>Unit) = {
    g(Source.fromFile(f))
    f.delete
  }
  def store_tempfile(f: FileWriter=>Unit)(implicit name:String) : JFile = {
    val tp=JFile.createTempFile(name,null)
    overwrite(tp,f)
    tp
  }

  implicit val name="bug2104"
  val chars=List('\n','\r','a')

  type Cs = List[Char]
  def all_strings(n: Int) : List[Cs] = {
    if (n==0) List(Nil)
    else {
      val sufs=all_strings(n-1)
      chars.flatMap((c)=>sufs.map(c :: _))
    }
  }
  def test(n: Int) {
    for(l <- all_strings(n)) {
      val tmp=store_tempfile((f)=>l.foreach(f.write(_)))
      delete_after(tmp,(s)=>assert(s.toList == l))
    }
  }
  def main(args: Array[String]) {
    (0 until N).foreach(test(_))
  }
}
