package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound

import scala.language.implicitConversions

trait Parameter {
  type Value
  def ->(v: Value): (this.type, Value) = (this, v)
}

sealed trait PMap {}

final case object PNil extends PMap {
  // do not move this in the PMap trait!
  def ::[P <: Parameter](pkey: (P, P#Value)) = PCons[P, PNil.type](pkey._2, this)
}

final case class PCons[P <: Parameter, T <: PMap](head: P#Value, tail: T) extends PMap {
  import PMap._
  // do not move this in the PMap trait!
  def ::[H <: Parameter](pkey: (H, H#Value)) = PCons[H, P :: T](pkey._2, this)

  @implicitNotFound("The parameter map has no element of type ${S}")
  def apply[S <: Parameter](implicit ix: Index[S, P :: T]) = ix(this)

  @implicitNotFound("The parameter map has no element of type ${S}")
  def apply[S <: Parameter](s: S)(implicit ix: Index[S, P :: T]) = ix(this)
}

object PMap {
  type ::[P <: Parameter, T <: PMap] = PCons[P, T]
  type PNil = PNil.type

  sealed trait Index[S <: Parameter, M <: PMap] {
    def apply(m: M): S#Value
  }

  sealed trait PMapConversion[S <: PMap, T <: PMap] {
    def apply(m: S): T
  }

  implicit def conversion[S <: PMap, T <: PMap](m: S)(implicit conv: PMapConversion[S, T]): T = conv(m)

  implicit def indexed0[S <: Parameter, H <: Parameter, T <: PMap](implicit ev: S =:= H) = new Index[S, H :: T] {
    def apply(m: H :: T) = m.head.asInstanceOf[S#Value]
  }

  implicit def indexedN[S <: Parameter, H <: Parameter, T <: PMap](implicit index: Index[S, T]) = new Index[S, H :: T] {
    def apply(m: H :: T) = index(m.tail)
  }

  implicit def pnilconversion[S <: PMap] = new PMapConversion[S, PNil] {
    def apply(m: S): PNil = PNil
  }

  implicit def pconsconversion[S <: PMap, H <: Parameter, M <: PMap](implicit ev: PMapConversion[S, M], ind: Index[H, S]) = new PMapConversion[S, H :: M] {
    def apply(m: S): H :: M = PCons[H, M](ind(m), ev(m))
  }
}
