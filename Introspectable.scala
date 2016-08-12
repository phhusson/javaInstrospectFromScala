import scala.language.dynamics
class Introspectable(val self: AnyRef) extends Dynamic {
  val c = self.getClass
  def field(s: String) = {
    val f = c.getField(s)
    f.setAccessible(true)
    f
  }

  def selectDynamic(s: String) = {
    field(s).get(self)
  }

  def updateDynamic(s: String)(v: AnyRef) {
    field(s).set(self, v)
  }

  def crossProduct(l: List[List[Class[_]]]): List[List[Class[_]]] = {
    if(l.isEmpty) List(List())
    else if(l.tail.isEmpty)
      for(v <- l.head) yield List(v)
    else
      l.head.flatMap( v => crossProduct(l.tail).map( p => v :: p))
  }

  def applyDynamic(fnc: String)(args: Any*) = {
    val possibleArgsType: List[List[Class[_]]] = args.toList.map({
      //TODO: Other types
      case o: Integer => List(classOf[Int], classOf[java.lang.Integer]);
      case o: Boolean => List(classOf[Boolean], classOf[java.lang.Boolean]);
      case o: AnyRef => {
        //Always list first children-most type, then parent type, then interfaces
        val cl = o.getClass
        val interfaces = cl.getInterfaces.toList
        val uppers = Iterator.iterate[Class[_]](cl)( _.getSuperclass).takeWhile( _ != null).toList
        uppers ++ interfaces
      }
    })

    val possiblePrototypes = crossProduct(possibleArgsType).map( _.toSeq)
    val prototype = possiblePrototypes.find( proto => try { c.getMethod(fnc, proto:_*); true } catch { case e: Exception => false })
    val method = c.getMethod(fnc, prototype.get:_*)

    val newArgs: Seq[AnyRef] = args.map({
      //Every cases actually goes into AnyRef because of implicit boxing
      case o: AnyRef => o
      case _ => { throw new Exception("Oopps, this shouldn't happen..."); null }
    })
    method.invoke(self, newArgs:_*)
  }
}