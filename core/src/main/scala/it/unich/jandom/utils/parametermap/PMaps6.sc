import scala.language.implicitConversions

object PMaps6 {
  import it.unich.jandom.utils.parametermap.PMaps6._

  object A extends Parameter { type Value = Int }

  object B extends Parameter { type Value = String }

  object C extends Parameter { type Value = Int }

  val y = cons((B -> "pippo"),cons((C -> 5),PNil))//> y  : it.unich.jandom.utils.parametermap.PMaps6.PMap{type PList = it.unich.ja
                                                  //| ndom.utils.parametermap.TLists.TCons[PMaps6.B.type,it.unich.jandom.utils.par
                                                  //| ametermap.PMaps6.PMap{type PList = it.unich.jandom.utils.parametermap.TLists
                                                  //| .TCons[PMaps6.C.type,it.unich.jandom.utils.parametermap.PMaps6.PMap{type PLi
                                                  //| st = it.unich.jandom.utils.parametermap.TLists.TNil}#PList]}#PList]} = it.un
                                                  //| ich.jandom.utils.parametermap.PMaps6$$anon$2@8e819b3

  val x = cons((A -> 3),y)                        //> x  : it.unich.jandom.utils.parametermap.PMaps6.PMap{type PList = it.unich.ja
                                                  //| ndom.utils.parametermap.TLists.TCons[PMaps6.A.type,it.unich.jandom.utils.par
                                                  //| ametermap.PMaps6.PMap{type PList = it.unich.jandom.utils.parametermap.TLists
                                                  //| .TCons[PMaps6.B.type,it.unich.jandom.utils.parametermap.PMaps6.PMap{type PLi
                                                  //| st = it.unich.jandom.utils.parametermap.TLists.TCons[PMaps6.C.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps6.PMap{type PList = it.unich.jandom.utils.par
                                                  //| ametermap.TLists.TNil}#PList]}#PList]}#PList]} = it.unich.jandom.utils.param
                                                  //| etermap.PMaps6$$anon$2@39582e88
  
  y(B)                                            //> res0: PMaps6.B.Value = pippo
  y(C)                                            //> res1: PMaps6.C.Value = 5
  x(A)                                            //> res2: PMaps6.A.Value = 3
  x(B)                                            //> res3: PMaps6.B.Value = pippo
  x(C)                                            //> res4: PMaps6.C.Value = 5

  y.get(A)                                        //> res5: Option[PMaps6.A.Value] = None

  def f(x: C.type :: PNil) = {
    x(C)
  }                                               //> f: (x: it.unich.jandom.utils.parametermap.PMaps6.::[PMaps6.C.type,it.unich.j
                                                  //| andom.utils.parametermap.PMaps6.PNil])PMaps6.C.Value

  def ff[S <% C.type :: PNil](x: S) = {
    x(C)
  }                                               //> ff: [S](x: S)(implicit evidence$1: S => it.unich.jandom.utils.parametermap.P
                                                  //| Maps6.::[PMaps6.C.type,it.unich.jandom.utils.parametermap.PMaps6.PNil])PMaps
                                                  //| 6.C.Value

  //f(y)
/*
  f(conv(x)) // does not work
  f(x)

  ff(y)
  ff(x)
*/
}