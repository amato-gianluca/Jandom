import scala.language.implicitConversions

object PMaps4sc {
  import it.unich.jandom.utils.parametermap.PMaps4._

  object A extends Parameter { type Value = Int }

  object B extends Parameter { type Value = String }

  val y = (A -> 3) :: PNil                        //> y  : it.unich.jandom.utils.parametermap.PMaps4.PCons[PMaps4sc.A.type,it.unic
                                                  //| h.jandom.utils.parametermap.PMaps4.PNil] = it.unich.jandom.utils.parameterma
                                                  //| p.PMaps4$PCons@76c197b6

  val x = (B -> "pippo") :: y                     //> x  : it.unich.jandom.utils.parametermap.PMaps4.PCons[PMaps4sc.B.type,it.unic
                                                  //| h.jandom.utils.parametermap.PMaps4.::[PMaps4sc.A.type,it.unich.jandom.utils.
                                                  //| parametermap.PMaps4.PNil]] = it.unich.jandom.utils.parametermap.PMaps4$PCons
                                                  //| @7a1b16bd

  x(A)                                            //> res0: PMaps4sc.A.Value = 3
  x(B)                                            //> res1: PMaps4sc.B.Value = pippo
  y(A)                                            //> res2: PMaps4sc.A.Value = 3

  y.get(B)                                        //> res3: Option[PMaps4sc.B.Value] = None

  def f(x: A.type :: PNil) = {
    x(A)
  }                                               //> f: (x: it.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.A.type,it.unich
                                                  //| .jandom.utils.parametermap.PMaps4.PNil])PMaps4sc.A.Value

  def g[S <: PMap](x: S)(implicit conv: Converter[S, A.type :: PNil]) = {
    conv(x)(A)
  }                                               //> g: [S <: it.unich.jandom.utils.parametermap.PMaps4.PMap](x: S)(implicit conv
                                                  //| : it.unich.jandom.utils.parametermap.PMaps4.Converter[S,it.unich.jandom.util
                                                  //| s.parametermap.PMaps4.::[PMaps4sc.A.type,it.unich.jandom.utils.parametermap.
                                                  //| PMaps4.PNil]])PMaps4sc.A.Value

  def ff[S <% A.type :: PNil](x: S) = {
    x(A)
  }                                               //> ff: [S](x: S)(implicit evidence$2: S => it.unich.jandom.utils.parametermap.P
                                                  //| Maps4.::[PMaps4sc.A.type,it.unich.jandom.utils.parametermap.PMaps4.PNil])PMa
                                                  //| ps4sc.A.Value

  def gg[S <: PMap](x: S)(implicit view: S => A.type :: PNil) = {
    view(x)(A)
  }                                               //> gg: [S <: it.unich.jandom.utils.parametermap.PMaps4.PMap](x: S)(implicit vie
                                                  //| w: S => it.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.A.type,it.unic
                                                  //| h.jandom.utils.parametermap.PMaps4.PNil])PMaps4sc.A.Value

  f(y)                                            //> res4: PMaps4sc.A.Value = 3
  ff(y)                                           //> res5: PMaps4sc.A.Value = 3
  g(y)                                            //> res6: PMaps4sc.A.Value = 3
  gg(y)                                           //> res7: PMaps4sc.A.Value = 3

  // val subs = implicitly[SubSet[y.type, x.type]]  // not working

  val subs = implicitly[SubSet[(A.type :: PNil), (B.type :: A.type :: PNil)]]
                                                  //> subs  : it.unich.jandom.utils.parametermap.PMaps4.SubSet[it.unich.jandom.uti
                                                  //| ls.parametermap.PMaps4.::[PMaps4sc.A.type,it.unich.jandom.utils.parametermap
                                                  //| .PMaps4.PNil],it.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.B.type,i
                                                  //| t.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.A.type,it.unich.jandom.
                                                  //| utils.parametermap.PMaps4.PNil]]] = null

  val myconvertyer = implicitly[Converter[B.type :: A.type :: PNil, A.type :: PNil]]
                                                  //> myconvertyer  : it.unich.jandom.utils.parametermap.PMaps4.Converter[it.unich
                                                  //| .jandom.utils.parametermap.PMaps4.::[PMaps4sc.B.type,it.unich.jandom.utils.p
                                                  //| arametermap.PMaps4.::[PMaps4sc.A.type,it.unich.jandom.utils.parametermap.PMa
                                                  //| ps4.PNil]],it.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.A.type,it.u
                                                  //| nich.jandom.utils.parametermap.PMaps4.PNil]] = it.unich.jandom.utils.paramet
                                                  //| ermap.PMaps4$$anon$1@2717f065

  // val myconvmap = implicitly[Function1[B.type :: A.type :: PNil,A.type ::PNil]]  //not working
  
  val myconvmap = convmap[B.type :: A.type :: PNil, A.type::PNil]
                                                  //> myconvmap  : it.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.B.type,i
                                                  //| t.unich.jandom.utils.parametermap.PMaps4.::[PMaps4sc.A.type,it.unich.jandom
                                                  //| .utils.parametermap.PMaps4.PNil]] => it.unich.jandom.utils.parametermap.PMa
                                                  //| ps4.::[PMaps4sc.A.type,it.unich.jandom.utils.parametermap.PMaps4.PNil] = <f
                                                  //| unction1>

  f(conv[B.type :: A.type :: PNil, A.type :: PNil](x))
                                                  //> res8: PMaps4sc.A.Value = 3

  f(conv2[B.type :: A.type :: PNil, A.type :: PNil](x))
                                                  //> res9: PMaps4sc.A.Value = 3
     
  // f(conv2(x)) // not working
  // f(conv(x)) // not working
  f(conv(x)(subs))                                //> res10: PMaps4sc.A.Value = 3
  //f(x)
  
  //ff(x)

  g(x)                                            //> res11: PMaps4sc.A.Value = 3

  // gg(x) //not working
  gg(x)(myconvmap)                                //> res12: PMaps4sc.A.Value = 3

  //f(x.adapt)
  //f(x.adapt2)
  //f(x.adapt3)

}