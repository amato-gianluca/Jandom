import scala.language.implicitConversions

object PMaps2 {
  import it.unich.jandom.utils.parametermap.TLists._
  import it.unich.jandom.utils.parametermap.PMaps2._
  import it.unich.jandom.utils.parametermap.PMaps2.::


  object A extends Parameter { type Value = Int }

  object B extends Parameter { type Value = String }

  val z = (A -> 3) :: PNil                        //> z  : it.unich.jandom.utils.parametermap.PMaps2.PCons[PMaps2.A.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps2.PNil] = it.unich.jandom.utils.parametermap.
                                                  //| PMaps2$PCons@1c148bd4

  val w: B.type :: PNil = (B -> "pippo") :: PNil  //> w  : it.unich.jandom.utils.parametermap.PMaps2.::[PMaps2.B.type,it.unich.jan
                                                  //| dom.utils.parametermap.PMaps2.PNil] = it.unich.jandom.utils.parametermap.PMa
                                                  //| ps2$PCons@d029f4

  val y = (A -> 3) :: PNil                        //> y  : it.unich.jandom.utils.parametermap.PMaps2.PCons[PMaps2.A.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps2.PNil] = it.unich.jandom.utils.parametermap.
                                                  //| PMaps2$PCons@62725296

  val x = (B -> "pippo") :: y                     //> x  : it.unich.jandom.utils.parametermap.PMaps2.PCons[PMaps2.B.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps2.::[PMaps2.A.type,it.unich.jandom.utils.para
                                                  //| metermap.PMaps2.PNil]] = it.unich.jandom.utils.parametermap.PMaps2$PCons@395
                                                  //| 82e88

  x(A)                                            //> res0: PMaps2.A.Value = 3
  x(B)                                            //> res1: PMaps2.B.Value = pippo
  y(A)                                            //> res2: PMaps2.A.Value = 3

  y.get(B)                                        //> res3: Option[PMaps2.B.Value] = None

  def f(x: A.type :: PNil) = {
    x(A)
  }                                               //> f: (x: it.unich.jandom.utils.parametermap.PMaps2.::[PMaps2.A.type,it.unich.j
                                                  //| andom.utils.parametermap.PMaps2.PNil])PMaps2.A.Value
  f(y)                                            //> res4: PMaps2.A.Value = 3

  f(conv[B.type :: A.type :: PNil, A.type :: PNil](x))
                                                  //> res5: PMaps2.A.Value = 3

  val impl2 = implicitly[SubSet[y.PList, x.PList]]//> impl2  : it.unich.jandom.utils.parametermap.TLists.SubSet[PMaps2.y.PList,PMa
                                                  //| ps2.x.PList] = null

  val impl = implicitly[SubSet[(A.type :: PNil)#PList, (B.type :: A.type ::PNil)#PList]]
                                                  //> impl  : it.unich.jandom.utils.parametermap.TLists.SubSet[it.unich.jandom.uti
                                                  //| ls.parametermap.TLists.TCons[PMaps2.A.type,it.unich.jandom.utils.parameterma
                                                  //| p.TLists.TNil.type],it.unich.jandom.utils.parametermap.TLists.TCons[PMaps2.B
                                                  //| .type,it.unich.jandom.utils.parametermap.TLists.TCons[PMaps2.A.type,it.unich
                                                  //| .jandom.utils.parametermap.TLists.TNil.type]]] = null
  
  val h2 = conv[B.type :: A.type :: PNil, A.type :: PNil] _
                                                  //> h2  : it.unich.jandom.utils.parametermap.PMaps2.::[PMaps2.B.type,it.unich.ja
                                                  //| ndom.utils.parametermap.PMaps2.::[PMaps2.A.type,it.unich.jandom.utils.parame
                                                  //| termap.PMaps2.PNil]] => it.unich.jandom.utils.parametermap.PMaps2.::[PMaps2.
                                                  //| A.type,it.unich.jandom.utils.parametermap.PMaps2.PNil] = <function1>
  
  //f(x)
}