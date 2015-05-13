package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

object PMaps2 {
  
  trait Parameter {
    type Value
    def ->(v: Value): (this.type, Value) = (this, v)
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    type PList <: TList
    
    @implicitNotFound("The parameter map has no element of type ${P}")
    def apply[P <: Parameter](p: P)(implicit ev: Contains[P, PList]) = delegate(p).asInstanceOf[P#Value]
    def get[P <: Parameter](p: P) = delegate.get(p).asInstanceOf[Option[P#Value]]
  }

  object PNil extends PMap(Map.empty[Parameter, Any]) {
    type PList = TNil
    // do not move this in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, PNil](pkey, this)
  }

  final class PCons[H <: Parameter, T <: PMap](pkey: (H, H#Value), other: T) extends PMap(other.delegate + pkey) {
    type PList = TCons[H,T#PList]
    // do not move this in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, H :: T](pkey, this)
  }

  type PNil = PNil.type
  type ::[P <: Parameter, T <: PMap] = PCons[P, T]

  implicit def conv[S <: PMap, T <: PMap](m: S)(implicit ev: SubSet[T#PList, S#PList]): T = m.asInstanceOf[T] 
}
