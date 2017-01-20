all:
	mkdir -p build 
	scalac -d build Test.scala Introspectable.scala TestClass.java
	scala -cp build me.phh.introspect.Test

