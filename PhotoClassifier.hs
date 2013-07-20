module PhotoClassifier ( processDir
                         ,DateStuff
                         ,toDateStuffFromString )
where

import System.IO
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import System.Environment
import Graphics.Exif
import Data.String.Utils
import Data.Time.Calendar

data DateStuff = DateStuff (String, String, String) 
                    deriving (Show)

data PictureStuff = PictureStuff { picName :: String
                                   , picDate :: DateStuff }
                                   deriving (Show)

-- Constants
--sTART_DATE :: DateStuff 
--sTART_DATE = DateStuff ("2013", "02", "09")

-- Date extraction functions
exYear :: [String] -> String
exYear (x:xs) = x 

exMonth :: [String] -> String
exMonth (x:y:xs) = y 

exDay :: [String] -> String
exDay [x] = x
exDay (x:xs) = exDay xs 

toDateStuffFromString :: String -> DateStuff
toDateStuffFromString d = let dd = split "-" d in
    DateStuff ((exYear dd), (exMonth dd), (exDay dd))

toDayFromDateStuff :: DateStuff -> Day
toDayFromDateStuff (DateStuff (y,m,d)) = 
    fromGregorian (read y :: Integer)
                  (read m :: Int)
                  (read d :: Int)

-- calcWeek: Calculates the week number, starting from d2 until d1. If d2 happened before d1, then
-- the result will be the abs value, prepended with '_'. E.g 3 weeks since..., _3 weeks before
calcWeek :: DateStuff -> DateStuff -> String
calcWeek d2 d1 = toString $ floor (fromIntegral (diffDays (toDayFromDateStuff d2) (toDayFromDateStuff d1)) / 7.0)
    where
    toString :: Integer -> String
    toString i  
          | i < 0 = ("_" ++) $ show $ (i * (-1))
          | otherwise = show i

-- cleanExifDate :: converts a String into a DateStuff
cleanExifDate :: Maybe String -> DateStuff
--cleanExifDate Nothing = sTART_DATE
cleanExifDate (Just d) = toDateStuff $ head $ split " " d
    where
    toDateStuff :: String -> DateStuff
    toDateStuff s = let s' = split ":" s in
                        DateStuff (exYear s', exMonth s', exDay s')
-- end cleanExifDate 


-- classifyPhoto: Takes a PictureStuff, DateStuff, Week (String) and Output dir (String) and
-- puts the photo in the OUTPUT dir and creates links in the year-month, year-week subfolders
classifyPhoto :: PictureStuff -> DateStuff -> String -> IO ()
classifyPhoto p (DateStuff (y,m,d)) w = let f = takeFileName (picName p) in 
                                            let dout = takeDirectory (picName p) in 
                                                  cDir True (outDir dout)>>
                                                  copyFile (picName p) (outDirFile dout f) >>
                                                  cDir True (outDirYear dout y) >> 
                                                  cDir True (outDirMonth dout y m) >> 
                                                  cDir True (outDirWeek dout y w) >>
                                                  createSymbolicLink (outDirFile dout f) (joinPath [outDirMonth dout y m, f]) >> 
                                                  createSymbolicLink (outDirFile dout f) (joinPath [outDirWeek dout y w, f]) 
                                                      where
                                                      cDir = createDirectoryIfMissing
                                                      outDir d = joinPath [d, "output"]
                                                      outDirFile d f = joinPath [outDir d, f]
                                                      outDirYear d y = joinPath [outDir d, y]
                                                      outDirMonth d y m = joinPath [outDirYear d y, m]
                                                      outDirWeek d y w = joinPath [outDirYear d y, w]
processPhoto :: PictureStuff -> DateStuff -> IO ()
processPhoto p sd = putStrLn (picName p) >> 
                    putStrLn "Date " >>
                    print (picDate p) >>
                    putStrLn "Week " >> 

                    let w = (calcWeek (picDate p) (sd)) in
                        let d = picDate p in
                            print ("Name: " ++ (picName p)) >> 
                            print ("FileName: " ++ takeFileName (picName p)) >> 
                            print w >> 
                            classifyPhoto p d w  

sepLn :: String
sepLn = concat $ replicate 20 "="

printExif :: (String, DateStuff) -> IO()
printExif (file, sd) =
    do exif <- fromFile file
       putStr sepLn >> putStr "  Processing Image: "
       putStr file >> putStr " "
       putStrLn sepLn
       dt_tag <- getTag exif "DateTime"
       processPhoto (PictureStuff {picName = file, picDate = cleanExifDate dt_tag}) sd

printTag (n,v) = 
    putStrLn $ n ++ ": " ++ v

showMe :: FilePath -> IO ()
showMe x = putStrLn x

processDir :: FilePath -> FilePath -> String -> IO ()
processDir dir outDir startDate =
    do files <- getDirectoryContents dir
       putStr "Processing Directory "
       putStrLn dir

       let sd = toDateStuffFromString startDate
       mapM_ printExif (map (\f -> (dir ++ "/" ++ f, sd)) (filter (endswith ".jpg") files))
