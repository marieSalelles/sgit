package sgit.localChangeRepo

import java.nio.file.{Files, Paths}

import sgit.io.{ReadFile, WriteFile}
import sgit.objects.{Blob, StagedLine}

object StagedUpdating {

  /**
   * Update the current state of the staged file
   * @param files : the files which are currently added
   * @return : the files to add
   */
  def updateStagedFile (files : Seq[Blob]):Seq[Blob] = {
    val fileToAdd :Seq[Blob] = fileAlreadyStaged(files)
    //update the state of the staged file
    removeFileInStaged()
    removeOldFileVersion(fileToAdd)
    fileToAdd
  }

  /**
   * Search if the file added by the user are not already added to the staged in the older version.
   * @param files : files currently added
   */
  def removeOldFileVersion (files : Seq[Blob]):Unit = {
    //retrieve he file store in the staged file
    val stagedFiles :Option[Seq[StagedLine]]= ReadFile.readStaged()

    if (stagedFiles.isDefined) {
      val stagedNames :Seq[String] = stagedFiles.get.map(n => n.path)
      val fileNames: Seq[String] = files.map(f => f.path)
      //retrieve duplicate files
      val duplicateNames :Seq[String] = fileNames.intersect(stagedNames.distinct).distinct
      //retrieve blobs which are the same names that a file in staged
      val blobSameName :Seq[Blob]= files.filter(f => duplicateNames.contains(f.path))
      val shaBlobs :Seq[String] = blobSameName.map(b => b.sha)
      //staged file without file which is updated in the new add
      val stagedFilesOk :Seq[StagedLine] = stagedFiles
        .get
        .filterNot((sf: StagedLine) => { !shaBlobs.contains(sf.sha) && duplicateNames.contains(sf.path)})
      WriteFile.rewriteStaged(stagedFilesOk)
    }
  }

  /**
   * Search if a file in current added state is already in the staged state.
   * @param files :file thatt the user would like add
   * @return : the files to add in the staged file
   */
  def fileAlreadyStaged(files :Seq[Blob]) :Seq[Blob] = {
    //retrieve the files store in the staged file
    val stagedFiles: Option[List[StagedLine]] = ReadFile.readStaged()
    if (stagedFiles.isDefined) {
      //retrieve the sha key of the staged file
      val stagedFilesSha :Seq[String]= stagedFiles.get.map((sl :StagedLine) => sl.sha)
      files.filterNot(f => stagedFilesSha.contains(f.sha))
    } else files
  }

  /**
   * Search if a file in the staged file is deleted in the user repository
   */
  def removeFileInStaged() :Unit = {
    val stagedFiles :Option[Seq[StagedLine]] =  ReadFile.readStaged()
    if (stagedFiles.isDefined) {
      val fileStillPresent :Seq[StagedLine] = stagedFiles.get.filter( blob => Files.exists(Paths.get(blob.path)))
      WriteFile.rewriteStaged(fileStillPresent)
    }
  }
}
