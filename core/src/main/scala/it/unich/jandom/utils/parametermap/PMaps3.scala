package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.higherKinds
import scala.language.implicitConversions

import metascala.Booleans._
import metascala.Nats._
import metascala.Comparables._

object PMaps3 {
  trait Parameter {
    type Id <: Nat
    type CompareType = Parameter
    type Equals[P <: Parameter] = Id == P#Id
    type LessThan[P <: Parameter] = Id < P#Id
    type Value
    def ->(v: Value): (this.type, Value) = (this, v)
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    type Contains[P <: Parameter] <: Bool
    type IsSubMapOf[PM <: PMap] <: Bool

    @implicitNotFound("The parameter map has no element of type ${P}")
    def apply[P <: Parameter](p: P)(implicit ev: Contains[P] =:= True) = delegate(p).asInstanceOf[P#Value]
    def get[P <: Parameter](p: P) = delegate.get(p).asInstanceOf[Option[P#Value]]
  }

  final object PNil extends PMap(Map.empty[Parameter, Any]) {
    type Contains[P <: Parameter] = False
    type IsSubMapOf[PM <: PMap] = True

    // do not move this in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, PNil.type](pkey, this)
  }

  final class PCons[P <: Parameter, T <: PMap](pkey: (P, P#Value), other: T) extends PMap(other.delegate + pkey) {
    type Contains[P1 <: Parameter] = P#Equals[P1] || T#Contains[P1]
    type IsSubMapOf[PM <: PMap] = PM#Contains[P] && T#IsSubMapOf[PM]

    // do not move this in the PMap trait!
    def ::[H <: Parameter](pkey: (H, H#Value)) = new PCons[H, P :: T](pkey, this)
  }

  type ::[P <: Parameter, T <: PMap] = PCons[P, T]
  type PNil = PNil.type

  implicit def conv[S <: PMap, T <: PMap](m: S)(implicit ev: T#IsSubMapOf[S] =:= True): T = m.asInstanceOf[T]

}
