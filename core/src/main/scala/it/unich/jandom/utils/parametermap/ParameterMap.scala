package it.unich.jandom.utils.parametermap

import scala.collection.immutable.HashMap

class Parameter {
  type Val
  def -->(av: Val) = new KeyVal[this.type, Val](this,av)
}

object Parameter {
  def apply[AVal] = new Parameter { type Val = AVal }
}

class KeyVal[P <: Parameter, V] (val k: P, val v: V) {
  type Parameter = P
  type Val = V
}

trait PTag[P <: Parameter]

class PMap private (val m: HashMap[Parameter, Any]) {
  def +[P <: Parameter](kv: KeyVal[P,_]): this.type with PTag[kv.Parameter] = {
    val newmap = new PMap(m + (kv.k -> kv.v))
    newmap.asInstanceOf[this.type with PTag[kv.Parameter]]
  }
  def apply(p: Parameter)(implicit ev: this.type => PTag[p.type]) = m(p).asInstanceOf[p.Val]
}

object PMap {
  val empty = new PMap(HashMap.empty)
}
