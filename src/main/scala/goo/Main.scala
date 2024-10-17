package goo

class Foo(val id: String, val me: Foo)

@main def hello(): Unit = {
  val root: Root = Root(other=null)
  val (var_, bod) = lam(root)
  link(var_, bod)
  val ast = net_to_ast(root)
  println(ast.toString())
}
def msg = "I was compiled by Scala 3 "
