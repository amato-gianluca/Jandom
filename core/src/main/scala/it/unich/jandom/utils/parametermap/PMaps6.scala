package it.unich.jandom.utils.parametermap

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

object PMaps6 {

  abstract class Parameter {
    type Value
    def -->(v: Value) = new ParameterValue[this.type](this, v)
  }

  object Parameter {
    def apply[PVal] = new Parameter { type Value = PVal }
  }

  final class ParameterValue[P <: Parameter](val p: P, val v: P#Value) {
    type Param = P
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    outer =>
    type PList <: TList

    @implicitNotFound("The parameter map has no such element")
    def apply(p: Parameter)(implicit ev: Contains[p.type, PList]) = delegate(p).asInstanceOf[p.Value]
    def get(p: Parameter) = delegate.get(p).asInstanceOf[Option[p.Value]]
    def +:[P <: Parameter](pv: ParameterValue[P]) = new PMap(delegate + (pv.p -> pv.v)) { type PList = TCons[pv.Param, outer.PList] }
  }

  object PMap {
    val empty: PNil = new PMap(Map.empty) { type PList = TNil }
  }

  type PNil = PMap { type PList = TNil }
  type +:[H <: Parameter, T <: PMap] = PMap { type PList = TCons[H, T#PList] }

  implicit def conv[T <: TList](m: PMap)(implicit ev: SubSet[T, m.PList]) = m.asInstanceOf[PMap { type PList = T }]
}
