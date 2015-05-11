package it.unich.jandom.utils.parametermap2

import scala.annotation.implicitNotFound

import scala.language.implicitConversions

trait Parameter {
  type Value
  def ->(v: Value): (this.type, Value) = (this, v)
}

sealed trait PMap {
  val delegate: Map[Parameter, Any]
  
  def get[P <: Parameter](p: P) = delegate.get(p).asInstanceOf[Option[P#Value]]
}

final case object PNil extends PMap {
  // do not move this in the PMap trait!
  val delegate = Map.empty[Parameter, Any]
  def ::[P <: Parameter](pkey: (P, P#Value)) = PCons[P, PNil.type](pkey, this)
}

final case class PCons[P <: Parameter, T <: PMap](pkey: (P, P#Value), other: T) extends PMap {
  import PMap._
  
  val delegate = other.delegate + pkey
  
  // do not move this in the PMap trait!
  def ::[H <: Parameter](pkey: (H, H#Value)) = PCons[H, P :: T](pkey, this)

  @implicitNotFound("The parameter map has no element of type ${S}")
  def apply[S <: Parameter](s: S)(implicit ix: Index[S, P :: T]) = delegate(s).asInstanceOf[S#Value]
}

object PMap {
  type ::[P <: Parameter, T <: PMap] = PCons[P, T]
  type PNil = PNil.type

  sealed trait Index[S <: Parameter, M <: PMap] { }

  sealed trait PMapConversion[S <: PMap, T <: PMap] { }

  implicit def conversion[S <: PMap, T <: PMap](m: S)(implicit conv: PMapConversion[S, T]): T = m.asInstanceOf[T]

  implicit def indexed0[S <: Parameter, H <: Parameter, T <: PMap](implicit ev: S =:= H) = new Index[S, H :: T] { }
  
  implicit def indexedN[S <: Parameter, H <: Parameter, T <: PMap](implicit index: Index[S, T]) = new Index[S, H :: T] { }

  implicit def pnilconversion[S <: PMap] = new PMapConversion[S, PNil] { }
  
  implicit def pconsconversion[S <: PMap, H <: Parameter, M <: PMap](implicit ev: PMapConversion[S, M], ind: Index[H, S]) = new PMapConversion[S, H :: M] { }
}
