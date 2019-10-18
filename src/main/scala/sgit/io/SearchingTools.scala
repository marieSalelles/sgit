package sgit.io

import better.files._
import sgit.objects.{Blob, Commit, StagedLine}

import scala.annotation.tailrec

object SearchingTools {

  /**
   * Search the .sgit folder
   * @return true if the .sgit folder exists
   */
  def searchSgitFolder(): Boolean ={
    ".sgit/".toFile.exists
  }

  /**
   * Search the last commit
   * @return the sha key of the last commit if it exists.
   */
  def findLastCommit(): Option[String] = {
    val currentBranch: String = ReadFile.readHEAD()
    ReadFile.readHeads(currentBranch)
  }

  /**
   * Search if the branch exists
   * @param branchName : branch name
   * @return true if the branch exists
   */
  def searchBranch(branchName: String): Boolean= {
    (".sgit/refs/heads/" + branchName).toFile.exists
  }

  /**
   * Retrieve all the branches with the name and last commit
   * @return a list of branch names and content (last commit on this branch)
   */
  def searchAllBranches(): Seq[(String, String)] = {
    val allBranchFiles: Seq[File] = ".sgit/refs/heads/".toFile.children.toList
    allBranchFiles.map(branch => (branch.name, branch.contentAsString))
  }

  /**
   * Search if the tag exists
   * @param tagName : tag name
   * @return true if the tag exists
   */
  def searchTag(tagName: String): Boolean = {
    (".sgit/refs/tags/" + tagName).toFile.exists
  }

  /**
   * Retrieve all the tags with the name and commit
   * @return a list of tag names and content (commit)
   */
  def searchAllTags(): Option[Seq[(String, String)]] = {
    val allTagFiles: Seq[File] = ".sgit/refs/tags/".toFile.children.toList
    if (allTagFiles.nonEmpty) Some(allTagFiles.map(tag => (tag.name, tag.contentAsString)))
    else None
  }

  /**
   * Search if the commit exists
   * @param commitSha : commit sha key
   * @return true if the commit exists
   */
  def searchCommit(commitSha: String): Boolean = {
    (".sgit/objects/" + commitSha).toFile.exists
  }

  /**
   * Search the untracked files.
   * @param workingDirectory : the working directory folders and files
   * @return all the untracked files path
   */
  def searchedUntrackedFiles(workingDirectory: Seq[File], stagedFile: Option[Seq[StagedLine]]): Option[Seq[String]] = {
    val root = ".sgit/".toFile.parent
    //take all the files
    val wdFiles :Seq[File] = workingDirectory.filterNot(f=> f.isDirectory)

    if (stagedFile.nonEmpty) {
      //retrieve file path in staged file
      val stagedPaths: Seq[String] = stagedFile.get.map(sf => sf.path)
      //retrieve the untracked files which are not in the staged file
      val untrackedFilePaths: Seq[File] = wdFiles.filterNot(file => stagedPaths.contains(root.relativize(file).toString))
      if (untrackedFilePaths.isEmpty) findFileNotInCommit(wdFiles) else findFileNotInCommit(untrackedFilePaths)
    } else findFileNotInCommit(wdFiles)

  }

  /**
   * Find the files which are not in the last commit
   * @param files : file list
   * @return the files which are not in the last commit (so the untracked files according to the last commit)
   */
  def findFileNotInCommit(files: Seq[File]): Option[Seq[String]] = {
    val root = ".sgit/".toFile.parent
    //search if the last commit knows the files
    val lastCommit: Option[String] = findLastCommit()

    if (lastCommit.isDefined) {
      //commit content
      val fileCommit: Seq[String] = ReadFile.readCommit(lastCommit.get).map(f => f.path)
      val fileNotInCommit: Seq[File] = files.filterNot(file => fileCommit.contains(root.relativize(file).toString))
      if (fileNotInCommit.isEmpty) None else Some(fileNotInCommit.map(f => root.relativize(f).toString))
    } else Some(files.map(f => root.relativize(f).toString))

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
   * Search the deleted files
   * @param workingDirectory : the working directory folders and files
   * @param commit : the list of files contains in the last commit
   * @return the deleted file path list
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
   * Search the modified files between the last commit and the working directory.
   * files classify into stages : Changes to be committed added and modified and Not staged for commit modified.
   * @param workingDirectory : working directory files
   * @param commitContent : commit files
   * @return the modified files (sha key and path)
   */
  def searchedModifiedFiles(workingDirectory: Seq[File], commitContent: Seq[StagedLine]): Option[Seq[StagedLine]] = {
    //take all the files
    val wdFiles :Seq[File] = workingDirectory.filterNot(f=> f.isDirectory)
    val root = ".sgit/".toFile.parent
    //retrieve path of commit files
    val commitFilesPath: Seq[String] = commitContent.map(f => f.path)
    //retrieve sha of commit files
    val commitFilesSha: Seq[String] = commitContent.map(f => f.sha)
    //file with same path, return files in the working directory
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
   * Search the different files between the working directory and the last commit.
   * @param workingDirectory : working directory files
   * @param commitContent : last commit files
   * @return a sequence of tuple which contains the blobs of the last commit file and blobs of the working directory file for the same file
   */
  def searchDifferentFileBetweenWDCommit(workingDirectory: Seq[File], commitContent: Seq[StagedLine]): Option[Seq[(Blob, Blob)]] = {
    //take all the working directory files
    val wdFiles :Seq[File] = workingDirectory.filterNot(f=> f.isDirectory)
    val root = ".sgit/".toFile.parent
    //retrieve path of commit files
    val commitFilesPath: Seq[String] = commitContent.map(f => f.path)
    //retrieve sha of commit files
    val commitFilesSha: Seq[String] = commitContent.map(f => f.sha)
    //files with same path, return files in the working directory
    val fileSamePath: Seq[File] = wdFiles.filter( f => commitFilesPath.contains(root.relativize(f).toString))

    //retrieve the different files
    val differentFilesBlob: Seq[(Blob,Blob)]= fileSamePath.iterator.map(f =>
      if(!commitFilesSha.contains(f.sha1)){
        val tuple = retrieveFilesWDCommit(root.relativize(f).toString,commitContent, wdFiles)
        if (tuple.isDefined) tuple.get
        else null
      } else null
    ).filterNot(f => f == null).toList

    if (differentFilesBlob.nonEmpty) Some(differentFilesBlob)
    else None
  }

  /**
   * Retrieve the same file into the last commit and the working directory
   * @param path: file path
   * @param commitContent : last commit content
   * @param wdFiles : working directory files
   * @return a tuple of blobs with in first the file contains in the commit and in second the file contains in the working directory
   */
  def retrieveFilesWDCommit(path: String, commitContent: Seq[StagedLine], wdFiles: Seq[File]): Option[(Blob,Blob)] = {
    //retrieve the file in the commit
    val sameFileCommit: Seq[StagedLine] = commitContent.filter(cf => cf.path == path)

    //retrieve the file in staged file
    val fileInStaged: Option[Seq[StagedLine]] = retrieveFileInStagedWithPath(path)

    //if the file is in the staged file , return the file version contains in the staged file
    if(fileInStaged.isDefined && fileInStaged.get.nonEmpty){
      val root = ".sgit/".toFile.parent

      //retrieve the file in the working directory
      val sameFileWD: Seq[File] = wdFiles.filter(wdf => root.relativize(wdf).toString == path)

      if(sameFileWD.nonEmpty){
        //retrieve the content of the staged file version
        val contentStagedFile = ReadFile.readBlobContent(fileInStaged.get.head.sha)
        Some((Blob(fileInStaged.get.head.sha, contentStagedFile, fileInStaged.get.head.path),
          Blob(sameFileWD.head.sha1, sameFileWD.head.contentAsString, root.relativize(sameFileWD.head).toString)))
      } else None

    } else if (sameFileCommit.nonEmpty) {
      val root = ".sgit/".toFile.parent

      //retrieve the file in the working directory
      val sameFileWD: Seq[File] = wdFiles.filter(wdf => root.relativize(wdf).toString == path)

      if(sameFileWD.nonEmpty){
        //retrieve the content of the commit file version
        val contentCommitFile = ReadFile.readBlobContent(sameFileCommit.head.sha)
        Some((Blob(sameFileCommit.head.sha, contentCommitFile, sameFileCommit.head.path),
          Blob(sameFileWD.head.sha1, sameFileWD.head.contentAsString, root.relativize(sameFileWD.head).toString)))
      } else None
    } else None
  }

  /**
   * Retrieve a file in staged file if the file is in it
   * @param path file path
   * @return the file if exists
   */
  def retrieveFileInStagedWithPath(path: String): Option[Seq[StagedLine]]= {
    ///retrieve the staged file content
    val stagedContent: Option[List[StagedLine]] = ReadFile.readStaged()
    //search if the file is add by the user (in the staged file)
    if(stagedContent.isDefined) {
      val file = stagedContent.get.filter(sf => sf.path == path)
      if (file.nonEmpty) Some(file)
      else None
    } else None
  }

  /**
   * Search all the folders and files with a path
   * @param paths : sequence of file path
   * @param allFiles : all the retrieved files and folders
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

}
