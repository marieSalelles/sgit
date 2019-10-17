package sgit.app.commandlineParser

case class Config (command: String = "",
                   option: String = "",
                   files: Seq[String] = Seq(),
                   message: String = "",
                   branch: String = "",
                   tag: String ="",
                   checkout: String ="",
                   merge: String ="",
                   rebase: String=""
                  ){}
