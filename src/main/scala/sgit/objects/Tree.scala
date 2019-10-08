package sgit.objects

case class Tree(sha :String, children :Seq[TreeElement], path :String) extends TreeElement
