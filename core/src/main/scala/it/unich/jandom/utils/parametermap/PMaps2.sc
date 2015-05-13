import scala.language.implicitConversions

object PMaps2 {
  import it.unich.jandom.utils.parametermap.TLists._
  import it.unich.jandom.utils.parametermap.PMaps2._
  import it.unich.jandom.utils.parametermap.PMaps2.::

  object A extends Parameter { type Value = Int }

  object B extends Parameter { type Value = String }

  val z = (A -> 3) :: PNil

  val w: B.type :: PNil = (B -> "pippo") :: PNil

  val y = (A -> 3) :: PNil

  val x = (B -> "pippo") :: y

  x(A)
  x(B)
  y(A)

  y.get(B)

  def f(x: A.type :: PNil) = {
    x(A)
  }

  def g[S <: PMap](x: S)(implicit conv: Converter[S, A.type :: PNil]) = {
    conv(x)(A)
  }

  def ff[S <% A.type :: PNil](x: S) = {
    x(A)
  }

  def gg[S <: PMap](x: S)(implicit view: S => A.type :: PNil) = {
    view(x)(A)
  }

  f(y)
  g(y)
  gg(y)

  val impl2 = implicitly[SubSet[y.PList, x.PList]]

  val impl = implicitly[SubSet[(A.type :: PNil)#PList, (B.type :: A.type :: PNil)#PList]]

  val h1 = implicitly[Converter[B.type :: A.type :: PNil, A.type :: PNil]]

  //val h2 = implicitly[Function1[B.type :: A.type :: PNil,A.type ::PNil]]  not working
  val h2 = convmap[B.type :: A.type :: PNil, A.type :: PNil]

  f(conv[B.type :: A.type :: PNil, A.type :: PNil](x))

  f(conv2[B.type :: A.type :: PNil, A.type :: PNil](x))

  //f(conv2(x)) not working
  //f(conv(x)) not working

  // ff(x)
  ff(x)(h2)
  
  g(x)

  // gg(x) not working
  gg(x)(h2)
  
  //f(x.adapt)
    //f(x.adapt2)
  f(x.adapt3)

}