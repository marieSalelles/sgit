package sgit.io

import sgit.objects.{Blob, StagedLine}

object StateFileDefining {

  def updateStagedFile (files : Seq[Blob]) = {
    val fileToAdd = fileAlreadyStaged(files)
    removeOldFileVersion(fileToAdd)
    fileToAdd
  }

  /**
   * Search if the file added by the user are not already added to the staged in the older version.
   * @param files : files currently added
   * @return : the older version of a file added
   */
  def removeOldFileVersion (files : Seq[Blob]) = {
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
  def fileAlreadyStaged(files :Seq[Blob]) = {
    val stagedFiles: Option[List[StagedLine]] = ReadFile.readStaged()
    if (stagedFiles.isDefined) {
      val stagedFilesSha = stagedFiles.get.map((sl :StagedLine) => sl.sha)
      files.filterNot(f => stagedFilesSha.contains(f.sha))
    } else files
  }

  /**
   * Search if a file in the staged file is deleted in the user repository
   * @return :the sequence of file which are  in the user repo
   */
 /* def updateFileStaged() = {
    val stagedFiles =  ReadFile.readStaged()
    val stagedFileNames: Seq[String] = stagedFiles.map((f: String) => ReadFile.readName(f))
    stagedFileNames.filter( f => Files.exists(Paths.get(f)))
  }*/
}
