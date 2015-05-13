package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

object PMaps6 {
  
  trait Parameter {
    type Value
    def ->(v: Value): (this.type, Value) = (this, v)
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    outer =>
    type PList <: TList
    
    @implicitNotFound("The parameter map has no element of type ${P}")
    def apply[P <: Parameter](p: P)(implicit ev: Contains[P, PList]) = delegate(p).asInstanceOf[P#Value]
    def get[P <: Parameter](p: P) = delegate.get(p).asInstanceOf[Option[P#Value]]
  }
  
  val PNil = new PMap(Map.empty) { type PList = TNil }
  type PNil = PNil.type
  
  def cons[P <: Parameter, T <: PMap](pkey: (P, P#Value), pmap: T) = new PMap(pmap.delegate + pkey) { type PList = TCons[P,T#PList] }
  type ::[H <: Parameter, T <: PMap] = PMap { type PList = TCons[H,T#PList] }
    
  implicit def conv[S <: PMap, T <: PMap](m: S)(implicit ev: SubSet[T#PList, S#PList]): T = m.asInstanceOf[T]
}
