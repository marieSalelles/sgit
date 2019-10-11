package sgit.io

import better.files._
import sgit.objects.{Commit, StagedLine}

import scala.annotation.tailrec

object SearchingTools {

  def searchSgitFolder(): Boolean ={
    ".sgit/".toFile.exists
  }

  /**
   * Search the untracked files.
   * @param workingDirectory : the working directory folders and files
   * @return all the untracked files
   */
  def searchedUntrackedFiles(workingDirectory: Seq[File]): Option[Seq[File]] = {
    val wdFiles :Seq[File] = workingDirectory.filterNot(f=> f.isDirectory)
    val untrackedFile: Seq[File] = wdFiles.filterNot(f => ("./sgit/objects/"+ f.sha1).toFile.exists)
    if (untrackedFile.isEmpty) None
    else Some(untrackedFile)
  }

  /**
   * Retrieve the files which are modified, added.
   * @param addedFile : files that the user would like to add.
   * @param lastCommit : last commit
   * @return the file sequence of the files to add.
   */
  def findUnmodifyFiles(addedFile: Seq[File], lastCommit: Option[Commit]): Seq[File] = {
    val root = ".sgit/".toFile.parent
    if(lastCommit.isDefined){
      val commitContent: Seq[StagedLine] = lastCommit.get.files
      val commitNames: Seq[String] = commitContent.map(f => f.path)
      addedFile.filterNot(f=> commitNames.contains(root.relativize(f).toString))
    } else addedFile
  }

  /**
   * Search the deleted file
   * @param workingDirectory : the working directory folders and files
   * @param stagedFiles : the list of files contains in the staged file
   * @return the deleted file
   */
  /*def searchDeletedFiles(workingDirectory: Seq[File], stagedFiles: Seq[StagedLine]): Option[Seq[File]] = {
    val staged = stagedFiles.map(f => f.path.toFile)
    val deletedFiles = staged.filterNot(f => workingDirectory.contains(f))
    if (deletedFiles.isEmpty) None
    else Some(deletedFiles)
  }*/

  /**
   * Search all the folder in a path
   * @param paths : sequence of file path
   * @param allFiles : all the files and folders
   * @return all the files and folders
   */
  @tailrec
  def retrieveFoldersWithPath(paths: Seq[String], allFiles: Seq[File]) :Seq[File] = {
    val path :String = paths.head
    if (paths.isEmpty) allFiles
    else if (path.toFile.exists) {
      val nodes: Seq[String] = path.replace("\\","/").split("/").toList
      val nodeFiles :Seq[File] = nodes.map(n => n.toFile)
      val newNodes: Seq[File] = nodeFiles.filterNot(file => allFiles.contains(file))
      retrieveFoldersWithPath(paths.tail, allFiles.concat(newNodes))
    }
    else retrieveFoldersWithPath(paths.tail, allFiles)
  }

  /**
   * Search folder children in a sequence of folders/files
   * @param nodes : sequence of folders/files
   * @param node : origin folder
   * @param children : children list of the node folder
   * @return children list of the node folder
   */
 /* @tailrec
  def findChildren(nodes :Seq[File], node :File, children: Seq[File]) :Seq[File] = {
    val n = nodes.head
    if (n.path.toString.contains(node.path.toString)) children :+ n
    else findChildren(nodes.tail, node, children)
  }*/

}
