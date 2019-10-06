package sgit.io

import better.files._

object WriteFile {

  /**
   * Write in HEAD file the current branch
   * @param name : branch name
   */
  def writeHead(name :String) = {
    val _: File = ".sgit/HEAD".toFile.append("ref:refs/heads/master")
  }

  def writeStaged(files :String) = {

  }
}
