/* Why does this code fail? b has type a.type, so the third
** declaration in S3 should be okay... The compiler writes instead:
**
** found   : S3.this.b.type (with underlying type S3)
** required: S3.this.a.type
**        val c: a.type = b;
**                        ^
** Without declaration 3, everything is fine.
*/
class S3() {
    val a = new S3();
    val b: a.type = a;
    val c: a.type = b;
}
