package sgit.io

import better.files._
import sgit.objects.{Blob, StagedLine}

import scala.annotation.tailrec

object WriteFile {

  /**
   * Write in HEAD file the current branch
   * @param name : branch name
   */
  def writeHead(name :String) = {
    val _: File = ".sgit/HEAD".toFile.overwrite("refs/heads/"+ name)
  }

  /**
   * Write in staged file
   * @param files : sequence of the files to add
   */
  def writeStaged(files :Seq[Blob]):Unit = {
      files.map(f => {
        ".sgit/staged".toFile.appendLine(f.sha+ " "+f.path)
      })
  }

  /**
   * Rewrite the staged file
   * @param files : the files to write
   */
  def rewriteStaged(files: Seq[StagedLine]) :Unit = {
    ".sgit/staged".toFile.overwrite("")
    files.map( f => {
      ".sgit/staged".toFile.appendLine(f.sha+ " "+f.path)
    })
  }

  /**
   * Clear the staged file.
   */
  def clearStaged() :Unit = {
    ".sgit/staged".toFile.overwrite("")
  }

  /**
   * Transform the sequence of stagedLine in a string
   * @param files : the stagedLine of staged file
   * @param res : string
   * @return the string with all the files
   */
  @tailrec
  def createListFileString(files :Seq[StagedLine], res: String) :String = {
    if (files.isEmpty) res
    else {
      val line: StagedLine = files.head
      createListFileString(files.tail, res + line.sha + " " + line.path + "\n")
    }
  }
}
