package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

object PMaps4 {

  trait Parameter {
    type Value
    def ->(v: Value): (this.type, Value) = (this, v)
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    @implicitNotFound("The parameter map has no element of type ${P}")
    def get[P <: Parameter](p: P) = delegate.get(p).asInstanceOf[Option[P#Value]]
    def adapt[M <: PMap](implicit ev: SubSet[M, this.type]) = this.asInstanceOf[M]
    def adapt2[M <: PMap](implicit ev: Converter[this.type, M]) = this.asInstanceOf[M]
  }

  final object PNil extends PMap(Map.empty[Parameter, Any]) {
    // do not move this in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, PNil](pkey, this)
  }

  final class PCons[H <: Parameter, T <: PMap](pkey: (H, H#Value), other: T) extends PMap(other.delegate + pkey) {
    // do not move this in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, H :: T](pkey, this)
    def apply[P <: Parameter](p: P)(implicit ev: Contains[P, H :: T]) = delegate(p).asInstanceOf[P#Value]
    def adapt3[M <: PMap](implicit ev: Converter[H :: T, M]) = this.asInstanceOf[M]
  }

  type PNil = PNil.type
  type ::[P <: Parameter, T <: PMap] = PCons[P, T]

  abstract class Converter[S <: PMap, T <: PMap] {
    def apply(m: S): T
  }

  implicit def convC[S <: PMap, T <: PMap](implicit ev: SubSet[T, S]) = new Converter[S, T] {
    def apply(m: S) = m.asInstanceOf[T]
  }

  def value[T] = null.asInstanceOf[T]

  final class Contains[P <: Parameter, M <: PMap]
  implicit def contains[H <: Parameter, T <: PMap] = value[Contains[H, H :: T]]
  implicit def contains2[P <: Parameter, H <: Parameter, T  <: PMap](implicit ev: Contains[P, T]) = value[Contains[P, H :: T]]

  final class SubSet[S <: PMap, T <: PMap]
  implicit def subset[T <: PMap] = value[SubSet[PNil, T]]
  implicit def subset2[H <: Parameter, S <: PMap, T <: PMap](implicit ev1: Contains[H, T], ev2: SubSet[S,T]) = value[SubSet[H :: S, T]]

  implicit def conv[S <: PMap, T <: PMap](m: S)(implicit ev: SubSet[T, S]): T = m.asInstanceOf[T]
  implicit def conv2[S <: PMap, T <: PMap](m: S)(implicit c: Converter[S, T]): T = m.asInstanceOf[T]
  implicit def convmap[S <: PMap, T <: PMap](implicit c: SubSet[T, S]): Function1[S,T] = { (m:S) => m.asInstanceOf[T] }

}
