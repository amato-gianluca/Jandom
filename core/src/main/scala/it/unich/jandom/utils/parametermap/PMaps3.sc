object PMaps3 {
  import it.unich.jandom.utils.parametermap.PMaps3._
  
  import metascala.Nats._
  import metascala.Booleans._

  object A extends Parameter {
    type Id = _0
    type Value = Int
  }

  object B extends Parameter {
    type Id = _1
    type Value = String
  }

  val z = (A -> 3) :: PNil                        //> z  : it.unich.jandom.utils.parametermap.PMaps3.PCons[PMaps3.A.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps3.PNil.type] = it.unich.jandom.utils.paramete
                                                  //| rmap.PMaps3$PCons@2d3d0953

  val w: B.type :: PNil = (B -> "pippo") :: PNil  //> w  : it.unich.jandom.utils.parametermap.PMaps3.::[PMaps3.B.type,it.unich.jan
                                                  //| dom.utils.parametermap.PMaps3.PNil] = it.unich.jandom.utils.parametermap.PMa
                                                  //| ps3$PCons@7adbe76f

  val y = (A -> 3) :: PNil                        //> y  : it.unich.jandom.utils.parametermap.PMaps3.PCons[PMaps3.A.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps3.PNil.type] = it.unich.jandom.utils.paramete
                                                  //| rmap.PMaps3$PCons@7b37676c
  
  val x = (B -> "pippo") ::  (A -> 3) :: PNil     //> x  : it.unich.jandom.utils.parametermap.PMaps3.PCons[PMaps3.B.type,it.unich.
                                                  //| jandom.utils.parametermap.PMaps3.::[PMaps3.A.type,it.unich.jandom.utils.para
                                                  //| metermap.PMaps3.PNil.type]] = it.unich.jandom.utils.parametermap.PMaps3$PCon
                                                  //| s@52dd1d62
  
  val im = implicitly[x.Contains[B.type] =:= True]//> im  : =:=[PMaps3.x.Contains[PMaps3.B.type],metascala.Booleans.True] = <funct
                                                  //| ion1>
  
  x(A)                                            //> res0: PMaps3.A.Value = 3
  x(B)                                            //> res1: PMaps3.B.Value = pippo
  y(A)                                            //> res2: PMaps3.A.Value = 3
  
  y.get(B)                                        //> res3: Option[PMaps3.B.Value] = None


  def f(x: A.type :: PNil) = {
     x(A)
  }                                               //> f: (x: it.unich.jandom.utils.parametermap.PMaps3.::[PMaps3.A.type,it.unich.j
                                                  //| andom.utils.parametermap.PMaps3.PNil])PMaps3.A.Value
  f(y)                                            //> res4: PMaps3.A.Value = 3

  //f(conv[B.type :: A.type :: PNil, A.type :: PNil](x))

//	f(x)
/*
  implicit val c = implicitly[PMapConversion[B.type :: A.type :: PNil, A.type :: PNil]]
                                                  
  // f(x)
 
  val ps1 = pconsconversion[A.type :: PNil, A.type, PNil]
                                                    
  val ps2 = pconsconversion[B.type :: A.type :: PNil, A.type, PNil]
  */
}