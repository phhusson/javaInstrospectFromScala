package me.phh.introspect

object Test {
  def runTestOn(t: AnyRef) {
    val o = new Introspectable(t)

    //Checks precedence of primitive type over boxed type
    o.totoFnc2(true)

    //Checks method-matching based on interface
    o.testInterface(t)

    //Checks extraction of object from an Introspectable
    o.testInterface(o)

    //Checks variable reading and assignment
    if(o.totoStr != "titi") throw new Exception();
    o.totoStr = "tata"
    if(o.totoStr != "tata") throw new Exception();

    o.testNull(classOf[String])
  }
  def runTestInterfaces() {
    val o = new Introspectable(Introspectable.create("me.phh.introspect.TestClass"))
    val o2 = Introspectable.create("me.phh.introspect.TestClass2")
    o.testInterface(o2)
  }
  def main(args: Array[String]) {
    val t = new TestClass()
    runTestOn(t)

    val t2 = Introspectable.create("me.phh.introspect.TestClass")
    runTestOn(t2)

    val t3 = Introspectable.create("me.phh.introspect.TestClass", 3)
    runTestOn(t3)

    if(new Introspectable(Class.forName("me.phh.introspect.TestClass")).staticFnc1() != 4) throw new Exception()

    runTestInterfaces()

    try {
      new Introspectable(t3).thisfunctiondoesntexist()
      throw new Exception()
    } catch {
      case e: java.lang.NoSuchMethodException => ()
    }
  }
}
