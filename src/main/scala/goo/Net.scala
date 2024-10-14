package goo

sealed trait PosPort { var other: NegPort }
case class PosPri(var node: PNode, var other: NegPort) extends PosPort
case class PosAux(var node: Node, var other: NegPort) extends PosPort

sealed trait NegPort { var other: PosPort }
case class Root (var other: PosPort) extends NegPort
case class NegPri(var node: NNode, var other: PosPort) extends NegPort
case class NegAux(var node: Node, var other: PosPort) extends NegPort

sealed trait Node

sealed trait PNode extends Node
case class Era(a: PosPri) extends PNode
case class Lam(a: PosPri, var var_ : PosAux, bod: NegAux) extends PNode
case class Sup(a: PosPri, fst: NegAux, snd: NegAux) extends PNode

sealed trait NNode extends Node
case class Del(a: NegPri) extends NNode
case class App(a: NegPri, arg: NegAux, res: PosAux) extends NNode
case class Dup(a: NegPri, fst: PosAux, snd: PosAux) extends NNode

def del(port: PosPort): Unit = {
  lazy val delPort = NegPri(node = delNode, other = port)
  lazy val delNode: NNode = Del(delPort)
  port.other = delPort
}

def era(port: NegPort): Unit = {
  lazy val eraPort = PosPri(node = eraNode, other = port)
  lazy val eraNode: PNode = Era(eraPort)
  port.other = eraPort
}

def link (p: PosPort, n: NegPort): Unit = {
  p.other = n;
  n.other = p;
}

def aux_wire(node0: Node, node1: Node): (PosAux, NegAux) = {
  lazy val p: PosAux = PosAux(node = node0, other=n)
  lazy val n = NegAux(node = node1, other=p)
  (p, n)
}

def dup(x: PosPort, x0: PosAux, x1: PosAux): Dup = {
  lazy val dup: Dup = Dup(pri, x0, x1)
  lazy val pri = NegPri(node = dup, other = x)
  x.other = pri
  dup
}

def sup(x: NegPort, x0: NegAux, x1: NegAux): Sup = {
  lazy val sup: Sup = Sup(pri, x0, x1)
  lazy val pri = PosPri(node = sup, other = x)
  x.other = pri
  sup
}

def lam(x: NegPort, var_ : PosAux, bod: NegAux): Lam = {
  lazy val lam: Lam = Lam(pri, var_, bod)
  lazy val pri = PosPri(node = lam, other = x)
  x.other = pri
  lam
}

def app(x: PosPort, arg: NegAux, res: PosAux): App = {
  lazy val app: App = App(pri, arg, res)
  lazy val pri = NegPri(node = app, other = x)
  x.other = pri
  app
}

def interact(p: PNode, n: NNode): Unit = {
  (p, n) match {
    case (Era(a), Del(b)) => {
      // do nothing
    }
    case (Era(a), App(b, arg, res)) => {
      del(arg.other)
      era(res.other)
    }
    case (Era(a), Dup(b, fst, snd)) => {
      era(fst.other);
      era(snd.other);
    }

    case (Lam(a, var_, bod), Del(b)) => {
      del(bod.other);
      era(var_.other);
    }
    case (Lam(a, var_, bod), App(b, arg, res)) => {
      link(arg.other, var_.other);
      link(bod.other, res.other);
    }
    case (Lam(a, var_, bod), Dup(b, fst, snd)) => {
      lazy val d: Dup = dup(bod.other, bod0p, bod1p)
      lazy val s: Sup = sup(var_.other, var0n, var1n)
      lazy val lam0: Lam = lam(fst.other, var0p, bod0n)
      lazy val lam1: Lam = lam(snd.other, var1p, bod1n)
      lazy val (bod0p, bod0n) = aux_wire(d, lam0)
      lazy val (bod1p, bod1n) = aux_wire(d, lam1)
      lazy val (var0p, var0n) = aux_wire(s, lam0)
      lazy val (var1p, var1n) = aux_wire(s, lam1)
    }

    case (Sup(a, fst, snd), Del(b)) => {
      del(fst.other);
      del(snd.other);
    }
    case (Sup(a, fst, snd), App(b, arg, res)) => {
      lazy val d: Dup = dup(arg.other, arg0p, arg1p)
      lazy val s: Sup = sup(res.other, res0n, res1n)
      lazy val app0: App = app(fst.other, arg0n, res0p)
      lazy val app1: App = app(snd.other, arg1n, res1p)
      lazy val (arg0p, arg0n) = aux_wire(d, app0)
      lazy val (arg1p, arg1n) = aux_wire(d, app0)
      lazy val (res0p, res0n) = aux_wire(app0, s)
      lazy val (res1p, res1n) = aux_wire(app1, s)
    }
    case (Sup(a, fstSup, sndSup), Dup(b, fstDup, sndDup)) => {
      link(fstSup.other, fstDup.other);
      link(sndSup.other, sndDup.other);
    }
  }
}
