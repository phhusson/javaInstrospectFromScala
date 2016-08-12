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
  }
  def main(args: Array[String]) {
    val t = new TestClass()
    runTestOn(t)

    val t2 = Introspectable.create("TestClass")
    runTestOn(t2)

    val t3 = Introspectable.create("TestClass", 3)
    runTestOn(t3)
  }
}
