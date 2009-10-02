object ObjectHolder {
    private[ObjectHolder] class PrivateObject
    def getPrivateObject = new PrivateObject
}

object Test {
    def main(args: Array[String]) {
        // compiler error: class PrivateObject cannot be accessed
        // in object test.ObjectHolder
        val a: ObjectHolder.PrivateObject = ObjectHolder.getPrivateObject

        // works fine
        val b = ObjectHolder.getPrivateObject
        println(b.getClass)
    }
}
/*
When declaring objects as private[package/object] or protected[package/object] it is possible to leak out references to these objects into the public api (can be desirable, this in itself is not a problem).

When users of the api receive such private object via a function call, they can create a variable to reference the private object using inferred typing:

val b = getPrivateObject()

However they cannot create such variable using declared typing:

val a: PrivateObject? = getPrivateObject()

The line above will generate a compiler error: "class PrivateObject? cannot be accessed". Which makes sense, because PrivateObject? was declared private. But in this case inferred typing should not work either, otherwise the behaviors of inferred typing and declared typing become inconsistent. */
