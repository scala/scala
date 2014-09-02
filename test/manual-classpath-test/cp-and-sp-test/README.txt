Test for classpath implementations (covers directories, zips and jars both for classpath and sourcepath):

Run in terminal:

..\..\..\build\pack\bin\scalac -YclasspathImpl:flat -Ylog-classpath:true -d out -sourcepath c-src;zip-with-d-src.zip;commons-lang-2.6-sources.jar -classpath f-class;zip-with-h-class.zip;guava-18.0.jar Main.scala

And then:

..\..\..\build\pack\bin\scala -classpath out;f-class;zip-with-h-class.zip Main

////////////
TODO remove whole manual-classpath-test directory, when it won't be needed (e.g. when
everything will be okay and this branch will have chance to be merged into master)