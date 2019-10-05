package sgit.io

import better.files._

object writeFile {

  def writeHead(name :String) = {
    val _: File = ".sgit/HEAD".toFile.append("ref:refs/heads/master")
  }
}
