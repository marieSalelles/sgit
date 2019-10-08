package sgit.io

import better.files._
import sgit.objects.{Blob, StagedLine}

object WriteFile {

  /**
   * Write in HEAD file the current branch
   * @param name : branch name
   */
  def writeHead(name :String) = {
    val _: File = ".sgit/HEAD".toFile.append("ref:refs/heads/master")
  }

  /**
   * Write in staged file
   * @param fileSha : sequence of the object to add
   */
  def writeStaged(fileSha :Seq[Blob]):Unit = {
      fileSha.map(f => {
        ".sgit/staged".toFile.appendLine(f.sha+ " "+f.path)
      })
  }

  /**
   * Rewrite the staged file
   * @param files : updated staged file
   */
  def rewriteStaged(files: Seq[StagedLine]) :Unit = {
    ".sgit/staged".toFile.overwrite("")
    files.map( f => {
      ".sgit/staged".toFile.appendLine(f.sha+ " "+f.path)
    })
  }
}
