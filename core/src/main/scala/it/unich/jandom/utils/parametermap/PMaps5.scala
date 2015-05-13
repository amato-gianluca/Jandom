package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

object PMaps5 {

  trait Parameter {
    type Value
    def ->(v: Value): (this.type, Value) = (this, v)
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {    
    @implicitNotFound("The parameter map has no element of type ${P}")
    def get[P <: Parameter](p: P) = delegate.get(p).asInstanceOf[Option[P#Value]]
  }

  final class PNil(delegate: Map[Parameter, Any]) extends PMap(delegate) {
    // do not move these in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, PNil](pkey, this)
  }

  final class PCons[H <: Parameter, T <: PMap](pkey: (H, H#Value), other: T) extends PMap(other.delegate + pkey) {
    // do not move these in the PMap trait!
    def ::[P <: Parameter](pkey: (P, P#Value)) = new PCons[P, H :: T](pkey, this)
    def apply[P <: Parameter](p: P)(implicit ev: Contains[P, H :: T]) = delegate(p).asInstanceOf[P#Value]
  }

  object PMap {
    final class PMapConv[S <: PMap, T <: PMap] extends Function1[S, T] {
      def apply(m: S): T = m.asInstanceOf[T]
    }

    // Put converions here since they have greater priority
    implicit def convPMap1[S <: PMap] = new PMapConv[S, PNil]
    implicit def convPMap2[S <: PMap, H <: Parameter, T <: PMap](implicit ev1: Contains[H, S], ev2: PMapConv[S, T]) = new PMapConv[S, H :: T]

    implicit def conv1[S <: PMap](m: S) = PNil
    implicit def conv2[S <: PMap, H <: Parameter, T <: PMap](m: S)(implicit ev1: Contains[H, S], ev2: PMapConv[S, T]) = m.asInstanceOf[H :: T]
  }

  val PNil = new PNil(Map.empty)
  type ::[P <: Parameter, T <: PMap] = PCons[P, T]

  def value[T] = null.asInstanceOf[T]

  final class Contains[P <: Parameter, M <: PMap]
  implicit def contains[H <: Parameter, T <: PMap] = value[Contains[H, H :: T]]
  implicit def contains2[P <: Parameter, H <: Parameter, T <: PMap](implicit ev: Contains[P, T]) = value[Contains[P, H :: T]]
}
