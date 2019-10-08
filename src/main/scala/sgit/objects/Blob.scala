package sgit.objects

case class Blob(sha :String, content :String, path :String) extends TreeElement
