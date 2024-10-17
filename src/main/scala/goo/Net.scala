package goo

sealed trait Port {var other: Port}
sealed trait AuxPort extends Port {var other: Port; var name: Option[String]}

sealed trait PosPort extends Port { var other: NegPort }
case class PosPri(var node: PNode, var other: NegPort) extends PosPort
case class PosAux(var node: Node, var other: NegPort, var name: Option[String] = None) extends PosPort with AuxPort

sealed trait NegPort extends Port { var other: PosPort }
case class Root (var other: PosPort) extends NegPort
case class NegPri(var node: NNode, var other: PosPort) extends NegPort
case class NegAux(var node: Node, var other: PosPort, var name: Option[String] = None) extends NegPort with AuxPort


sealed trait Node

sealed trait PNode extends Node
case class Era(var a: PosPri) extends PNode
case class Lam(var a: PosPri, var var_ : PosAux, var bod: NegAux) extends PNode
case class Sup(var a: PosPri, var fst: NegAux, var snd: NegAux) extends PNode

sealed trait NNode extends Node
case class Del(var a: NegPri) extends NNode
case class App(var a: NegPri, var arg: NegAux, var res: PosAux) extends NNode
case class Dup(var a: NegPri, var fst: PosAux, var snd: PosAux) extends NNode

def del(port: PosPort): Unit = {
  val delPort = NegPri(node = null, other = port)
  val delNode = Del(delPort)
  delPort.node = delNode
  port.other = delPort
}

def era(port: NegPort): Unit = {
  val eraPort = PosPri(node = null, other = port)
  val eraNode = Era(eraPort)
  eraPort.node = eraNode
  port.other = eraPort
}

def link (p: PosPort, n: NegPort): Unit = {
  p.other = n;
  n.other = p;
}

def dup(x: PosPort): (PosPort, PosPort) = {
  val dup: Dup = Dup(null, null, null)
  dup.a = NegPri(node = dup, other = x)
  dup.fst = PosAux(node = dup, other = null)
  dup.snd = PosAux(node = dup, other = null)
  x.other = dup.a
  (dup.fst, dup.snd)
}

def sup(x: NegPort): (NegPort, NegPort) = {
  val sup: Sup = Sup(null, null, null)
  sup.a = PosPri(node = sup, other = x)
  sup.fst = NegAux(node = sup, other = null)
  sup.snd = NegAux(node = sup, other = null)
  x.other = sup.a
  (sup.fst, sup.snd)
}

def lam(x: NegPort): (PosPort, NegPort) = {
  val lamNode = Lam(null, null, null)
  lamNode.a = PosPri(lamNode, x) // Initialize other to null for now
  lamNode.var_ = PosAux(lamNode, null) // Initialize other to null for now
  lamNode.bod = NegAux(lamNode, null) // Initialize other to null for now

  x.other = lamNode.a
  // Update the other references
  (lamNode.var_, lamNode.bod)
}

def app(x: PosPort): (NegPort, PosPort) = {
  val appNode = App(null, null, null)
  appNode.a = NegPri(appNode, x) // Initialize other to null for now
  appNode.arg = NegAux(appNode, null) // Initialize other to null for now
  appNode.res = PosAux(appNode, null) // Initialize other to null for now

  // Link the ports (update other references)
  x.other = appNode.a
  // Update the other references
  (appNode.arg, appNode.res) // Return with initialized others
}

def interact(p: PNode, n: NNode): Unit = {
  (p, n) match {
    case (Era(a), Del(b)) =>
      // Do nothing

    case (Era(a), App(b, arg, res)) =>
      del(arg.other)
      era(res.other)

    case (Era(a), Dup(b, fst, snd)) =>
      era(fst.other)
      era(snd.other)

    case (Lam(a, varAux, bod), Del(b)) =>
      del(bod.other)
      era(varAux.other)

    case (Lam(a, varAux, bod), App(b, arg, res)) =>
      link(arg.other, varAux.other)
      link(bod.other, res.other)

    case (Lam(a, varAux, bod), Dup(b, fst, snd)) =>
      val (bod0, bod1) = dup(bod.other)
      val (var0, var1) = sup(varAux.other)
      val (lam0_var, lam0_bod) = lam(fst.other)
      val (lam1_var, lam1_bod) = lam(snd.other)
      link(bod0, lam0_bod)
      link(bod1, lam1_bod)
      link(lam0_var, var0)
      link(lam1_var, var1)

    case (Sup(a, fst, snd), Del(b)) =>
      del(fst.other)
      del(snd.other)

    case (Sup(a, fstSup, sndSup), App(b, arg, res)) =>
      val (arg0, arg1) = dup(arg.other)
      val (res0, res1) = sup(res.other)
      val (app0_arg, app0_res) = app(fstSup.other)
      val (app1_arg, app1_res) = app(sndSup.other)
      link(arg0, app0_arg)
      link(arg1, app1_arg)
      link(app0_res, res0)
      link(app1_res, res1)

    case (Sup(a, fstSup, sndSup), Dup(b, fstDup, sndDup)) =>
      link(fstSup.other, fstDup.other)
      link(sndSup.other, sndDup.other)
  }
}
