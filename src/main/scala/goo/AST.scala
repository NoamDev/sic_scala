package goo

sealed trait ASTNode
case class ASTEra() extends ASTNode
case class ASTDel() extends ASTNode
case class ASTLam(var_ : ASTNode, bod: ASTNode) extends ASTNode
case class ASTApp(arg: ASTNode, res: ASTNode) extends ASTNode
case class ASTDup(fst: ASTNode, snd: ASTNode) extends ASTNode
case class ASTSup(fst: ASTNode, snd: ASTNode) extends ASTNode
case class ASTVar(name: String) extends ASTNode

val ALPHABET : String = "abcdefghijklmnopqrstuvwxyz"


def alphabetize(idx: Int): String = {
  @scala.annotation.tailrec
  def loop(n: Int, acc: String): String = {
    if (n < 0) acc
    else {
      val letter = ALPHABET(n % 26)
      loop((n / 26) - 1, letter + acc)
    }
  }
  loop(idx, "")
}

sealed class NameGen(var counter: Int = 0) {
    def next(): String = {
        counter += 1
        alphabetize(counter - 1)
    }
}


def net_to_ast (port: Port, name_gen: NameGen = NameGen()): ASTNode = {
    port match
        case Root(other) => net_to_ast(other, name_gen)
        case PosPri(node, other) => {
            node match
                case Era(a) => ASTEra()
                case Lam(a, var_, bod) =>
                    ASTLam(net_to_ast(var_, name_gen), net_to_ast(bod, name_gen))
                case Sup(a, fst, snd) =>
                    ASTSup(net_to_ast(fst, name_gen), net_to_ast(snd, name_gen))
        }
        case NegPri(node, other) => {
            node match
                case Del(a) => ASTDel()
                case App(a, arg, res) =>
                    ASTApp(net_to_ast(arg, name_gen), net_to_ast(res, name_gen))
                case Dup(a, fst, snd) =>
                    ASTDup(net_to_ast(fst, name_gen), net_to_ast(snd, name_gen))
        }
        case aux: AuxPort =>
            aux.name match
                case None => aux.other match
                    case aux2: AuxPort => {
                        val name = name_gen.next()
                        aux.name = Some(name)
                        aux2.name = Some(name)
                        ASTVar(name)
                    }
                    case port2 => net_to_ast(port2, name_gen)
                case Some(value) => ASTVar(value)
}