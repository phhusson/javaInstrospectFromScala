all:
	mkdir -p build 
	javac -d build TestClass.java
	scalac -d build Test.scala Introspectable.scala TestClass.java
	scala -cp build me.phh.introspect.Test

