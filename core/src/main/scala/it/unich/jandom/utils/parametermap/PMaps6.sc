import scala.language.implicitConversions

object PMaps6 {
  import it.unich.jandom.utils.parametermap.TLists._
  import it.unich.jandom.utils.parametermap.PMaps6._

  object A extends Parameter { type Value = Int }

  val B = Parameter[String]                       //> B  : it.unich.jandom.utils.parametermap.PMaps6.Parameter{type Value = String
                                                  //| } = it.unich.jandom.utils.parametermap.PMaps6$Parameter$$anon$1@13b2f057

  object C extends Parameter { type Value = Int }

  val D = Parameter[String]                       //> D  : it.unich.jandom.utils.parametermap.PMaps6.Parameter{type Value = String
                                                  //| } = it.unich.jandom.utils.parametermap.PMaps6$Parameter$$anon$1@49e1d547
                                                  
  val y = (B --> "pippo") +: (C --> 5) +: PMap.empty
                                                  //> y  : it.unich.jandom.utils.parametermap.PMaps6.PMap{type PList = it.unich.ja
                                                  //| ndom.utils.parametermap.TLists.TCons[it.unich.jandom.utils.parametermap.PMap
                                                  //| s6.Parameter.<refinement>.type,it.unich.jandom.utils.parametermap.TLists.TCo
                                                  //| ns[PMaps6.C.type,it.unich.jandom.utils.parametermap.TLists.TNil.type]]} = it
                                                  //| .unich.jandom.utils.parametermap.PMaps6$PMap$$anon$2@73b61aea
  val x = (A --> 3) +: y                          //> x  : it.unich.jandom.utils.parametermap.PMaps6.PMap{type PList = it.unich.ja
                                                  //| ndom.utils.parametermap.TLists.TCons[PMaps6.A.type,PMaps6.y.PList]} = it.uni
                                                  //| ch.jandom.utils.parametermap.PMaps6$PMap$$anon$2@2e5231c4

  y(B)                                            //> res0: PMaps6.B.Value = pippo
  y(C)                                            //> res1: PMaps6.C.Value = 5
  x(A)                                            //> res2: PMaps6.A.Value = 3
  x(B)                                            //> res3: PMaps6.B.Value = pippo
  x(C)                                            //> res4: PMaps6.C.Value = 5

  y.get(A)                                        //> res5: Option[PMaps6.A.Value] = None
  
  //y(D)
  //x(D)
  //y(A)
  
  def f(x: B.type +: A.type +: PNil) = {
    x(A)
    x(B)
  }                                               //> f: (x: it.unich.jandom.utils.parametermap.PMaps6.+:[it.unich.jandom.utils.pa
                                                  //| rametermap.PMaps6.Parameter.<refinement>.type,it.unich.jandom.utils.paramete
                                                  //| rmap.PMaps6.+:[PMaps6.A.type,it.unich.jandom.utils.parametermap.PMaps6.PNil]
                                                  //| ])PMaps6.B.Value
  def f2(x: C.type +: A.type +: PNil) = {
    x(A)
    x(C)
  }                                               //> f2: (x: it.unich.jandom.utils.parametermap.PMaps6.+:[PMaps6.C.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps6.+:[PMaps6.A.type,it.unich.jandom.utils.para
                                                  //| metermap.PMaps6.PNil]])PMaps6.C.Value
  f(x)                                            //> res6: PMaps6.B.Value = pippo
  f2(x)                                           //> res7: PMaps6.C.Value = 5

  
  def tryInference[H <: Parameter](p: H):H = p    //> tryInference: [H <: it.unich.jandom.utils.parametermap.PMaps6.Parameter](p: 
                                                  //| H)H
  
  tryInference(B)                                 //> res8: it.unich.jandom.utils.parametermap.PMaps6.Parameter{type Value = Strin
                                                  //| g} = it.unich.jandom.utils.parametermap.PMaps6$Parameter$$anon$1@13b2f057

}