package sgit.io

import java.nio.file.Paths

import better.files._
import sgit.objects.{Blob, Commit, StagedLine}

import scala.annotation.tailrec

object CreateFile {

  /**
   * Create the file in the objects folder corresponding to the file staged.
   * @param files : files lists
   * @return : sequence of sha keys
   */
  def createObjectBlob(files :Seq[File]) :Seq[Blob] = {
    val root = ".sgit/".toFile.parent
    files.map((file :File) => {
      val path = Paths.get(".sgit/objects/")
      val _: File=  (path + "/" + file.sha1)
        .toFile
         .appendLine(root.relativize(file).toString)
        .appendText(file.contentAsString)
      Blob(file.sha1,file.contentAsString, root.relativize(file).toString)
    })
  }

  /**
   * Create a commit file.
   * @param parents parents of the new commit
   * @return the sha key of the commit file
   */
  def createCommit(parents: (String, String), content: Seq[StagedLine], message: String) :String ={
      val commitFile: File =".sgit/objects/temporaryFile".toFile
        .createIfNotExists()
        .appendLine(parents._1 + " " + parents._2)
        .appendLine(message)
        .appendText(WriteFile.createListFileString(content, ""))
      commitFile.renameTo(commitFile.sha1).sha1
  }

  /**
   * Create the file in the objects folder corresponding to the folder modified
   * @param folders : folder list
   * @return : sequence of sha keys
   */
  /*def createObjectTree(folders :Seq[File]) :Seq[String] = {

  }*/

}
