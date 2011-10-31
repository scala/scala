class LazyValVsFunctionType[a]  {
    val f: a => a = x => {
        lazy val _x: a = throw new java.lang.Error("todo")
        _x // error: type mismatch
/*
[error]  found   : a => => a
[error]  required: a => a
[error]     val f: a => a = x => {
[error]                       ^
[error] one error found
*/
        // _x: a // ok
    }
}