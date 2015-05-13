import scala.language.implicitConversions

object PMaps5sc {
  import it.unich.jandom.utils.parametermap.PMaps5._

  object A extends Parameter { type Value = Int }

  object B extends Parameter { type Value = String }

  object C extends Parameter { type Value = Int }

  val y = (B -> "pippo") :: (C -> 5) :: PNil      //> y  : it.unich.jandom.utils.parametermap.PMaps5.PCons[PMaps5sc.B.type,it.unic
                                                  //| h.jandom.utils.parametermap.PMaps5.::[PMaps5sc.C.type,it.unich.jandom.utils.
                                                  //| parametermap.PMaps5.PNil]] = it.unich.jandom.utils.parametermap.PMaps5$PCons
                                                  //| @37611a05

  val x = (A -> 3) :: y                           //> x  : it.unich.jandom.utils.parametermap.PMaps5.PCons[PMaps5sc.A.type,it.unic
                                                  //| h.jandom.utils.parametermap.PMaps5.::[PMaps5sc.B.type,it.unich.jandom.utils.
                                                  //| parametermap.PMaps5.::[PMaps5sc.C.type,it.unich.jandom.utils.parametermap.PM
                                                  //| aps5.PNil]]] = it.unich.jandom.utils.parametermap.PMaps5$PCons@6679fcae
  y(B)                                            //> res0: PMaps5sc.B.Value = pippo
  y(C)                                            //> res1: PMaps5sc.C.Value = 5
  x(A)                                            //> res2: PMaps5sc.A.Value = 3
  x(B)                                            //> res3: PMaps5sc.B.Value = pippo
  x(C)                                            //> res4: PMaps5sc.C.Value = 5

  y.get(A)                                        //> res5: Option[PMaps5sc.A.Value] = None

  def f(x: C.type :: PNil) = {
    x(C)
  }                                               //> f: (x: it.unich.jandom.utils.parametermap.PMaps5.::[PMaps5sc.C.type,it.unich
                                                  //| .jandom.utils.parametermap.PMaps5.PNil])PMaps5sc.C.Value

  def ff[S <% C.type :: PNil](x: S) = {
    x(C)
  }                                               //> ff: [S](x: S)(implicit evidence$1: S => it.unich.jandom.utils.parametermap.P
                                                  //| Maps5.::[PMaps5sc.C.type,it.unich.jandom.utils.parametermap.PMaps5.PNil])PMa
                                                  //| ps5sc.C.Value

  f(y)                                            //> res6: PMaps5sc.C.Value = 5
  f(conv2(x)) // does not work                    //> res7: PMaps5sc.C.Value = 5
  f(x)                                            //> res8: PMaps5sc.C.Value = 5

  ff(y)                                           //> res9: PMaps5sc.C.Value = 5
  ff(x)                                           //> res10: PMaps5sc.C.Value = 5

}