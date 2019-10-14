package sgit.app.commandlineParser

import java.io.File

case class Config (command: String = "",
                   option: String = "",
                   files: Seq[String] = Seq(),
                   message: String = "",
                   branch: String = ""
                  ){}
