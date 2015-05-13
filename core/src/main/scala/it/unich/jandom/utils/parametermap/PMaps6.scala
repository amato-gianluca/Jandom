package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

object PMaps6 {

  abstract class Parameter {
    this: Singleton =>
    type Value
    def -->(v: Value): (this.type, Value) = (this, v)
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    outer =>
    type PList <: TList

    @implicitNotFound("This parameter map has no such element")
    def apply(p: Parameter)(implicit ev: Contains[p.type, PList]) = delegate(p).asInstanceOf[p.Value]
    def get(p: Parameter) = delegate.get(p).asInstanceOf[Option[p.Value]]
    def +:[P <: Parameter](pv: (P, P#Value)) = new PMap(delegate + pv) { type PList = TCons[P, outer.PList] }
  }

  object PMap {
    val empty: PNil = new PMap(Map.empty) { type PList = TNil }
  }

  type PNil = PMap { type PList = TNil }
  type +:[H <: Parameter, T <: PMap] = PMap { type PList = TCons[H, T#PList] }

  implicit def conv1[S <: PMap](m: S) = m.asInstanceOf[PNil]
  implicit def conv2[S <: PMap, H <: Parameter, T <: TList](m: PMap)(implicit ev: Contains[H, m.PList], ev2: SubSet[T, m.PList]) = m.asInstanceOf[PMap { type PList = TCons[H, T] }]
}
