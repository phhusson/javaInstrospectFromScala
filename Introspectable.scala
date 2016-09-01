package me.phh.introspect

import scala.language.dynamics

object Introspectable {
  def possibleArgsType(args: Any*): List[List[Class[_]]] = args.toList.map({
    //TODO: Other types
    case o: Integer => List(classOf[Int], classOf[java.lang.Integer]);
    case o: Boolean => List(classOf[Boolean], classOf[java.lang.Boolean]);
    case o: Long => List(classOf[Long], classOf[java.lang.Long]);
    case o: Byte => List(classOf[Byte], classOf[java.lang.Byte]);
    case o: Short => List(classOf[Short], classOf[java.lang.Short]);
    case o: Float => List(classOf[Float], classOf[java.lang.Float]);
    case o: Double => List(classOf[Double], classOf[java.lang.Double]);
    //case o: Char => List(classOf[Char], classOf[java.lang.Char]); // This boxed type doesn't exist...
    case o: AnyRef => {
      //Always list first children-most type, then parent type, then interfaces
      val cl = o.getClass
      val interfaces = cl.getInterfaces.toList
      val uppers = Iterator.iterate[Class[_]](cl)( _.getSuperclass).takeWhile( _ != null).toList
      uppers ++ interfaces
    }
  })

  def possiblePrototypes(args: Any*) =
    crossProduct(possibleArgsType(args:_*)).map( _.toSeq)

  def crossProduct(l: List[List[Class[_]]]): List[List[Class[_]]] = {
    if(l.isEmpty) List(List())
    else if(l.tail.isEmpty)
      for(v <- l.head) yield List(v)
    else
      l.head.flatMap( v => crossProduct(l.tail).map( p => v :: p))
  }

  def create(s: String, args: Any*): AnyRef = {
    val c = Class.forName(s)
    val selfExtracted = args.map({
      case o: Introspectable => o.self
      case o => o
    })

    val possibles = Introspectable.possiblePrototypes(selfExtracted:_*)
    val prototype = possibles
      .find( proto => try { c.getConstructor(proto:_*); true } catch { case e: Exception => false })
    val constructor = c.getConstructor(prototype.get:_*)

    val newArgs: Seq[AnyRef] = selfExtracted.map({
      //Every cases actually goes into AnyRef because of implicit boxing
      case o: AnyRef => o
      case _ => { throw new Exception("Oopps, this shouldn't happen..."); null }
    })
    constructor.newInstance(newArgs:_*).asInstanceOf[AnyRef]
  }
}

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

  def applyDynamic(fnc: String)(args: Any*) = {
    val selfExtracted = args.map({
      case o: Introspectable => o.self
      case o => o
    })
    val possibles = Introspectable.possiblePrototypes(selfExtracted:_*)
    val prototype = possibles
      .find( proto => try { c.getMethod(fnc, proto:_*); true } catch { case e: Exception => false })
    prototype match {
      case None => throw new java.lang.NoSuchMethodException(); null
      case Some(proto) => {
        val method = c.getMethod(fnc, prototype.get:_*)

        val newArgs: Seq[AnyRef] = selfExtracted.map({
          //Every cases actually goes into AnyRef because of implicit boxing
          case o: AnyRef => o
          case _ => { throw new Exception("Oopps, this shouldn't happen..."); null }
        })
        method.invoke(self, newArgs:_*)
      }
    }
  }
}
