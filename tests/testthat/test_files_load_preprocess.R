library(stringr)
context("Files loading")

sepr ='/';
if(.Platform$OS.type == "windows") {
  sepr = '\\';
}

test_that("flattenning of the file structure", {
  infold = 'TestFolderIn';
  in1 = file.path(infold, '1f');
  in2 = file.path(infold, '2f');
  outFold = 'TestFolderOut';
  dir.create(outFold, showWarnings = FALSE)
  dir.create(infold, showWarnings = FALSE)
  dir.create(in1, showWarnings = FALSE)
  dir.create(in2, showWarnings = FALSE)
  file.create(paste(in1, '1.txt', sep = sepr), showWarnings = TRUE)
  file.create(paste(in1, '2.txt', sep = sepr), showWarnings = TRUE)
  file.create(paste(in2, '3.txt', sep = sepr), showWarnings = TRUE)
  file.create(paste(in2, '4.txt', sep = sepr), showWarnings = TRUE)


  #Attention: folder path must end with / (\\)
  flatten.files.structure(paste(getwd(), infold, '', sep='/'), paste( getwd(), outFold, '', sep='/'))

  expect_equal(length(list.files(outFold)), 4)

})


test_that("preprocessing test", {
  infold = 'FlattenFilesForPreprocessing';
  outFold = 'Preprocessed';
  dir.create(outFold, showWarnings = FALSE)
  #Attention: folder path must end with / (\\)
  preprocess(paste(getwd(), infold, '', sep='/'), paste( getwd(), outFold, '', sep='/'))

  expect_equal(length(list.files(outFold)), length(list.files(infold)))

})



test_that("data Frame Creation", {
  infold = 'Preprocessed';
  outFold = 'dataFrame.df';
  #Attention: folder path must end with / (\\)

  create.articles.data.frame(paste(getwd(), infold, '', sep='/'), outFold)

  expect_that( file.exists(outFold), is_true() )

})

test_that("data Frame vectorization and binarization", {
  infold = 'dataFrame.df';

  df <- read.table(infold, header = TRUE);

  vdf = vectorize.data(df, 1, create.hash(c("c.OtherApplicationsNEC", "c.OtherSciencesNEC")), 1)
  print(vdf$fact)
  print(vdf$data[4])
  expect_equal(length(vdf$fact), 3 )

  save(vdf, file = 'articles.bin', compress = FALSE)

  bvdf = make.df.binary(vdf$data)
  print(bvdf[4])
  save(bvdf, file='binaryDF.bin', compress = FALSE)

})


