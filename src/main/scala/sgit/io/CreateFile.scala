package sgit.io

import better.files._
import sgit.objects.{Blob, StagedLine}

object CreateFile {

  /**
   * Create the file in the objects folder corresponding to the staged file.
   * @param files: staged files lists
   * @return: sequence of the file sha keys
   */
  def createObjectBlob(files :Seq[File]) :Seq[Blob] = {
    val root: File = ".sgit/".toFile.parent

    files.map((file :File) => {
      val path = (".sgit/objects/").toFile
      val _: File=  (path + "/" + file.sha1)
        .toFile
         .appendLine(root.relativize(file).toString)
        .appendText(file.contentAsString)
      Blob(file.sha1,file.contentAsString, root.relativize(file).toString)
    })
  }

  /**
   * (Re)write in the heads folder the file of the current branch
   * @param branch the current branch
   * @param commit the new commit
   */
  def writeHeadsFile(branch: String, commit: String):Unit = {
    (".sgit/"+branch).toFile.createIfNotExists().overwrite(commit)
  }

  /**
   * Write a tag file in the tags folder.
   * @param tag : tag name
   * @param commit : commit sha
   */
  def writeTagsFile(tag: String, commit: String): Unit = {
    (".sgit/"+ tag).toFile.createIfNotExists().overwrite(commit)
  }

  /**
   * Create a commit file.
   * @param parents parent(s) of the new commit
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
}
