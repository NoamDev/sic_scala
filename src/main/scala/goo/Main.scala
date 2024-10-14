package goo

@main def hello(): Unit =
  lazy val root: Root = Root(other=era_port)
  lazy val era: Era = Era(era_port)
  lazy val era_port = PosPri(node=era, other=root)
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3 "
