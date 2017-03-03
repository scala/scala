// done
// test/files/trait-defaults/fields.scala:24: error: double definition:
// def signalDelegate_=(x$1: Signalling): Unit at line 24 and
// def signalDelegate_=(x$1: Signalling): Unit at line 24
// have same type
// class SUB extends IterableSplitter
//       ^
// one error found

trait Signalling

trait DelegatedSignalling extends Signalling {
  var signalDelegate: Signalling
}

trait IterableSplitter extends DelegatedSignalling {
  var signalDelegate: Signalling = ???
}

class SUB extends IterableSplitter