object PMaps {
  import it.unich.jandom.utils.parametermap.PMaps._

  object A extends Parameter {
    type Value = Int
  }

  object B extends Parameter {
    type Value = String
  }

  val z = (A -> 3) :: PNil                        //> z  : it.unich.jandom.utils.parametermap.PMaps.PCons[PMaps.A.type,it.unich.ja
                                                  //| ndom.utils.parametermap.PMaps.PNil.type] = PCons(3,PNil)

  val w: B.type :: PNil = (B -> "pippo") :: PNil  //> w  : it.unich.jandom.utils.parametermap.PMaps.::[PMaps.B.type,it.unich.jando
                                                  //| m.utils.parametermap.PMaps.PNil] = PCons(pippo,PNil)

  val y = (A -> 3) :: PNil                        //> y  : it.unich.jandom.utils.parametermap.PMaps.PCons[PMaps.A.type,it.unich.ja
                                                  //| ndom.utils.parametermap.PMaps.PNil.type] = PCons(3,PNil)
  val x = (B -> "pippo") :: y                     //> x  : it.unich.jandom.utils.parametermap.PMaps.PCons[PMaps.B.type,it.unich.ja
                                                  //| ndom.utils.parametermap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.paramete
                                                  //| rmap.PMaps.PNil.type]] = PCons(pippo,PCons(3,PNil))

  val t = indexed0[A.type, A.type, PNil.type]     //> t  : it.unich.jandom.utils.parametermap.PMaps.Index[PMaps.A.type,it.unich.ja
                                                  //| ndom.utils.parametermap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.paramete
                                                  //| rmap.PMaps.PNil.type]] = it.unich.jandom.utils.parametermap.PMaps$$anon$1@15
                                                  //| f2cff0
  
  t(y)                                            //> res0: PMaps.A.Value = 3

  val t2 = indexed0[B.type, B.type, A.type :: PNil.type]
                                                  //> t2  : it.unich.jandom.utils.parametermap.PMaps.Index[PMaps.B.type,it.unich.j
                                                  //| andom.utils.parametermap.PMaps.::[PMaps.B.type,it.unich.jandom.utils.paramet
                                                  //| ermap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.parametermap.PMaps.PNil.ty
                                                  //| pe]]] = it.unich.jandom.utils.parametermap.PMaps$$anon$1@7831a0d1

  t2(x)                                           //> res1: PMaps.B.Value = pippo

  val t3 = indexedN[A.type, B.type, A.type :: PNil.type]
                                                  //> t3  : it.unich.jandom.utils.parametermap.PMaps.Index[PMaps.A.type,it.unich.j
                                                  //| andom.utils.parametermap.PMaps.::[PMaps.B.type,it.unich.jandom.utils.paramet
                                                  //| ermap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.parametermap.PMaps.PNil.ty
                                                  //| pe]]] = it.unich.jandom.utils.parametermap.PMaps$$anon$2@599a4de1

  x[A.type]                                       //> res2: PMaps.A.Value = 3
  x[B.type]                                       //> res3: PMaps.B.Value = pippo

  def f[T <% A.type :: PNil](x:T) = {
     x(A)
  }                                               //> f: [T](x: T)(implicit evidence$2: T => it.unich.jandom.utils.parametermap.PM
                                                  //| aps.::[PMaps.A.type,it.unich.jandom.utils.parametermap.PMaps.PNil])PMaps.A.V
                                                  //| alue
  f(y)                                            //> res4: PMaps.A.Value = 3
 
  f(conversion[B.type :: A.type :: PNil, A.type :: PNil](x))
                                                  //> res5: PMaps.A.Value = 3
                                                  
  //f(x)
 
  val ps1 = pconsconversion[A.type :: PNil, A.type, PNil]
                                                  //> ps1  : it.unich.jandom.utils.parametermap.PMaps.PMapConversion[it.unich.jand
                                                  //| om.utils.parametermap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.parameterm
                                                  //| ap.PMaps.PNil],it.unich.jandom.utils.parametermap.PMaps.::[PMaps.A.type,it.u
                                                  //| nich.jandom.utils.parametermap.PMaps.PNil]] = it.unich.jandom.utils.paramete
                                                  //| rmap.PMaps$$anon$4@368eb8bf
  
  ps1(y)                                          //> res6: it.unich.jandom.utils.parametermap.PMaps.::[PMaps.A.type,it.unich.jand
                                                  //| om.utils.parametermap.PMaps.PNil] = PCons(3,PNil)
                                                  
  val ps2 = pconsconversion[B.type :: A.type :: PNil, A.type, PNil]
                                                  //> ps2  : it.unich.jandom.utils.parametermap.PMaps.PMapConversion[it.unich.jand
                                                  //| om.utils.parametermap.PMaps.::[PMaps.B.type,it.unich.jandom.utils.parameterm
                                                  //| ap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.parametermap.PMaps.PNil]],it.
                                                  //| unich.jandom.utils.parametermap.PMaps.::[PMaps.A.type,it.unich.jandom.utils.
                                                  //| parametermap.PMaps.PNil]] = it.unich.jandom.utils.parametermap.PMaps$$anon$4
                                                  //| @54031f94
                                                  
  ps2(x)                                          //> res7: it.unich.jandom.utils.parametermap.PMaps.::[PMaps.A.type,it.unich.jand
                                                  //| om.utils.parametermap.PMaps.PNil] = PCons(3,PNil)
                                                     
}