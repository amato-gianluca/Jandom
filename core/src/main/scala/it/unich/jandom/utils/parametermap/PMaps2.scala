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
    def adapt[M <: PMap](implicit ev: SubSet[M#PList, PList]) = this.asInstanceOf[M]
    def adapt2[M <: PMap](implicit ev: Converter[this.type,M]) = this.asInstanceOf[M]
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
    def adapt3[M <: PMap](implicit ev: Converter[H :: T,M]) = this.asInstanceOf[M]
  }

  type PNil = PNil.type
  type ::[P <: Parameter, T <: PMap] = PCons[P, T]
  
  abstract class Converter[S <: PMap, T <: PMap] {
    def apply(m: S): T
  }
   
  implicit def convC[S <: PMap, T <: PMap](implicit ev: SubSet[T#PList, S#PList])= new Converter[S,T] {
    def apply(m: S) = m.asInstanceOf[T]
  } 
    
  implicit def conv[S <: PMap, T <: PMap](m: S)(implicit ev: SubSet[T#PList, S#PList]): T = m.asInstanceOf[T]
  
  implicit def conv2[S <: PMap, T <: PMap](m: S)(implicit c: Converter[S,T]): T = m.asInstanceOf[T]
  
  implicit def convmap[S <: PMap, T <: PMap](implicit ev: SubSet[T#PList, S#PList]): S => T =  { (m:S) => m.asInstanceOf[T] }
}
