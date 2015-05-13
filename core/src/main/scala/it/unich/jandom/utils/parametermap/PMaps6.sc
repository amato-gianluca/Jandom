import scala.language.implicitConversions

object PMaps6 {
  import it.unich.jandom.utils.parametermap.TLists._
  import it.unich.jandom.utils.parametermap.PMaps6._

  object A extends Parameter { type Value = Int }
  object B extends Parameter { type Value = String }

  object C extends Parameter { type Value = Int }
   
  val y = (B --> "pippo") +: (C --> 5) +: PMap.empty
  val x = (A --> 3) +: y

  y(B)
  y(C)
  x(A)
  x(B)
  x(C)

  y.get(A)
  //y(A)


  def f(x: B.type +: A.type +: PNil) = {
    x(A)
    x(B)
  }

  def ff[S <% C.type +: PNil](x: S) = {
    x(C)
  }

  val ss = implicitly[SubSet[TCons[B.type, TNil], y.PList]]
  //ff(y)
  //f(conv(y)(ss))
  // val z = conv2[B1.type :: C.type :: PNil, C.type, PNil](y)
  
  //f(conv2(y))
  //
  f(y)
  f(x)
  //ff(x)
}