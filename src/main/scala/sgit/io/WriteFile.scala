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
    val _: File = ".sgit/HEAD".toFile.append("refs/heads/master")
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

  /**
   * Clear the staged file.
   */
  def clearStaged() :Unit = {
    ".sgit/staged".toFile.overwrite("")
  }

  /**
   * Rewrite in the heads folder the file of the current branch
   * @param branch the current branch
   * @param commit the new commit
   */
  def writeHeadsFile(branch: String, commit: String):Unit = {
    (".sgit/"+branch).toFile.overwrite(commit)
  }

  /**
   * Transform the sequence of stagedLine in a string
   * @param files : the staged lines of staged file
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
