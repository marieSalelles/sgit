package sgit.io

import better.files._
import sgit.objects.{Commit, StagedLine}

import scala.annotation.tailrec

object SearchingTools {

  /**
   * Search the .sgit folder
   * @return true if the .sgit exists
   */
  def searchSgitFolder(): Boolean ={
    ".sgit/".toFile.exists
  }

  /**
   * Search the last commit
   * @return the sha key of the last commit if it exists.
   */
  def findLastCommit(): Option[String] = {
    val currentBranch = ReadFile.readHEAD()
    ReadFile.readHeads(currentBranch)
  }

  /**
   * Search the untracked files (it are not in the objects directory).
   * @param workingDirectory : the working directory folders and files
   * @return all the untracked files path
   */
  def searchedUntrackedFiles(workingDirectory: Seq[File], stagedFile: Option[Seq[StagedLine]]): Option[Seq[String]] = {
    val root = ".sgit/".toFile.parent
    //take all the files
    val wdFiles :Seq[File] = workingDirectory.filterNot(f=> f.isDirectory)
    //search the file with sha not in objects folder
    val untrackedFile: Seq[File] = wdFiles.filterNot(f => (".sgit/objects/"+ f.sha1).toFile.exists)

    if (stagedFile.nonEmpty) {
      //retrieve path of file in staged
      val stagedPaths: Seq[String] = stagedFile.get.map(sf => sf.path)
      //retrieve the untracked file which are not in the staged file
      val untrackedFilePaths: Seq[File] = untrackedFile.filterNot(file => stagedPaths.contains(root.relativize(file).toString))
      if (untrackedFilePaths.isEmpty) None else Some(untrackedFilePaths.map(f => root.relativize(f).toString))
      // return the working directory if no added files
    } else Some(untrackedFile.map(f => root.relativize(f).toString))
  }

  /**
   * Retrieve the files which are modified, added when the user add files.
   * Delete the unmodified files when the user adds some files.
   * @param addedFile : files that the user would like to add.
   * @param lastCommit : last commit
   * @return the file sequence of the files to add.
   */
  def findUnmodifyFiles(addedFile: Seq[File], lastCommit: Option[Commit]): Seq[File] = {
    val root = ".sgit/".toFile.parent
    if(lastCommit.isDefined){
      val commitContent: Seq[StagedLine] = lastCommit.get.files
      val commitFileNames: Seq[String] = commitContent.map(f => f.sha)
      val commitFilePaths: Seq[String] = commitContent.map(f => f.path)
      addedFile.filterNot(f=> (commitFilePaths.contains(root.relativize(f).toString) && commitFileNames.contains(f.sha1)))
    } else addedFile
  }

  /**
   * Search the deleted file
   * @param workingDirectory : the working directory folders and files
   * @param commit : the list of files contains in the last commit
   * @return the deleted file path
   */
  def searchDeletedFiles(workingDirectory: Seq[File], commit: Seq[StagedLine]): Option[Seq[String]] = {
    val commitContent: Seq[File] = commit.map(f => f.path.toFile)
    val deletedFiles: Seq[File] = commitContent.filterNot(f => workingDirectory.contains(f))

    if (deletedFiles.isEmpty) None
    else {
      val root = ".sgit/".toFile.parent
      Some(deletedFiles.map(f => root.relativize(f).toString))
    }
  }

  /**
   * Search the modified file between the last commit and the working directory.
   * files classify into stages : Changes to be committed added and modified and Not staged for commit modified.
   * @param workingDirectory : working directory files
   * @param commitContent : commit files
   * @return the modified files (sha and path)
   */
  def searchedModifiedFiles(workingDirectory: Seq[File], commitContent: Seq[StagedLine]): Option[Seq[StagedLine]] = {
    //take all the files
    val wdFiles :Seq[File] = workingDirectory.filterNot(f=> f.isDirectory)
    val root = ".sgit/".toFile.parent
    //retrieve path of commit files
    val commitFilesPath: Seq[String] = commitContent.map(f => f.path)
    //retrieve sha of commit files
    val commitFilesSha: Seq[String] = commitContent.map(f => f.sha)
    //file with same path, return file in the working directory
    val fileSamePath: Seq[File] = wdFiles.filter( f => commitFilesPath.contains(root.relativize(f).toString))
    //modified files
    val modifiedFiles: Seq[StagedLine] = fileSamePath
      .filterNot(cf => commitFilesSha.contains(cf.sha1))
      .map(file => StagedLine(file.sha1, root.relativize(file).toString))
    if (modifiedFiles.isEmpty) None else Some(modifiedFiles)
  }

  /**
   * Retrieve the files which are for the first time added by the user.
   * @param stagedFiles : files contain in the staged file.
   * @param commit : last commit files
   * @return the list of the files added for the first time by the user.
   */
  def toBeCommittedFileAdded(stagedFiles: Seq[StagedLine], commit: Seq[StagedLine]) :Option[Seq[StagedLine] ]= {
    val commitPath: Seq[String] = commit.map(c => c.path)
    val result: Seq[StagedLine] = stagedFiles.filterNot(sf => commitPath.contains(sf.path))
    if (result.isEmpty) None
    else Some(result)
  }

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
