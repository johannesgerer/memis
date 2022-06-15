{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Arrow
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.ByteString.UTF8 as B
import           Data.Char
import qualified Data.HashMap.Strict as M
import           Data.List hiding (find)
import           Data.List.Split
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types
import           Network.Mime
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Safe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.FilePath.Find
import           System.Process (proc,readProcessWithExitCode)
import           System.Process.ByteString.Lazy hiding (readProcessWithExitCode)
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Printf
import           Text.Read (readEither)
import           Text.Regex
import           Text.Regex.TDFA
import           Web.Frank
import           Web.Simple hiding (hoistEither,body)

port = 8081 :: Int
targetD = "/home/data/schriftverkehr/aktuell"
sourceD = "/home/data/schriftverkehr/Unsortiert"

javascript x = H.script H.! A.src x $ mempty
stylesheet x = H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href x

app2 :: String -> String -> Application
app2 target source = controllerApp () $ do
  get "/" $ do
    dirs <- lift $ targetDirs target
    img <- lift (images source)
    respond $ okHtml $ body dirs $ H.unsafeByteString $ mconcat
      ["var imgs = ", L.toStrict $ encode
       $ (\x -> M.fromList [("id"::String,x)]) <$> img ]
  get "/img" $ do
    f :: String  <- queryParam' "id"
    size :: Maybe String <- queryParam "size"
    let scale size = do
          (_,stdout,_) <- lift $ readCreateProcessWithExitCode
            (proc "convert" [f,"-filter","Lagrange"
                                     ,"-resize",size,"-"]) ""
          respond $ ok (defContentType f) stdout
    if source `isPrefixOf` f then maybe (respond $ file f) scale size
      else error $ printf "%s not in %s" f source
  post "/apply" $ do
    (params,_) <- parseForm
    let a = either (\x -> error $ "decode failed: " ++ x) id
          $ eitherDecodeStrict' $ lookupJust "imgs" params :: [Action]
        runIt x = E.handle (\e-> return $ show (e::E.SomeException)) $
                  x (target,source)
    respond . okHtml . mconcat . fmap L.fromString  =<< lift (mapM runIt a)
  get "/mkdir" $ do
    name :: String  <- queryParam' "name"
    resp <- lift $ E.handle
      (\e->return $ L.fromString $ show (e::E.SomeException)) $ do
        createDirectoryIfMissing True $ target </> name
        renderHtml <$> targetDirs target
    respond . okHtml $ resp


type Action = (String,String) -> IO String

instance FromJSON Action where
  parseJSON (Object v) = do
    old <- v .: "id"
    let fileO = takeFileName old
        convertDate "delete" = Right $ "delete_" ++ fileO
        convertDate s = (++ takeExtension old) . intercalate "-"
          <$> mapM form  (reverse . splitOn "." $ s)
        form d = printf "%02i" <$> left (printf "date '%s': %s" d :: String -> String)
          (readEither d :: Either String Int)
    file <- maybe (Right fileO) convertDate <$> v .:? "date"
    dir <- maybe (return $ takeDirectory old) (.: "id") =<< v .:? "dir"
    angle <- v .:? "angle"
    return $ \(target,source) -> do
      msg <- rotate old angle
      let rename f = if dir </> f == old then return "" else do
            new <- (dir </>) <$> getUnusedFilename [target,source] f
            exis <- doesFileExist new
            when (exis || not (target `isPrefixOf` new || source `isPrefixOf` new)) $
              error $ "something went wrong: " ++ show (old,dir,f,new)
            renameFile old new
            return $ printf "%s -> %s\n" old new
          rename :: String -> IO String
      return . (msg ++) =<< do
        either (return . printf "Error handling %s: %s\n" old) rename file



rotate :: String -> Maybe Int -> IO String
rotate _ Nothing = pure ""
rotate f (Just angle) = g <$> readProcessWithExitCode
  "gm" ["mogrify","-rotate", show angle, f] ""
  where g (ExitSuccess,_,_) = printf "Rotated by %s by %d degrees\n" f angle
        g (_,_,err) = err


getUnusedFilename dirs f = do
  l <- concat <$> forM dirs (find always $ fileType ==? RegularFile)
  return . r . succ . headDef 0 . sortBy (flip compare) . (g =<<) $ l
  where core [] = [takeBaseName f, "" , takeExtension f]
        core x = tail x
        [b,_,e] = core $ m f
        m = matches "(.*)_([0-9]+)(\\.[^.]+)?"
        r c = printf "%s_%03i%s"  b c e :: String
        g s = case m (takeFileName s) of
          [_,b2,c,e2] -> if b2 /= b || e2 /= e then []
                         else [read c :: Int]
          []          -> []

matches r s = getAllTextSubmatches $ (s::String) =~ (r:: String) :: [String]

body dirs script =renderHtml $ H.docTypeHtml $ do
  H.head $ do
    H.meta H.! A.charset "UTF-8"
    stylesheet "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/smoothness/jquery-ui.css"
    stylesheet "style.css"
    javascript "//ajax.googleapis.com/ajax/libs/jquery/2.2.2/jquery.min.js"
    javascript "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js"
    H.script script
    javascript "script.js"
  H.body $ do
    H.span  H.! A.id "largeSpan" $ do
      H.div ""
      H.span H.! A.id "input" $ ""
      H.h4 ""
      H.span H.! A.id "imgSpan" $ ""
      H.span H.! A.id "large" H.! A.class_ "imgBorder" $ ""
    H.span  H.! A.id "dirs" $ do
      H.h4 $ H.preEscapedString "&nbsp;"
      dirs
    H.span  H.! A.id "help" $ do
      H.h4 "Key Bindings"
      H.table $ mapM_ (\(a, b) -> H.tr $ H.td a >> H.td b)
        [("[Left]","Previous Image")
        ,("[Right]","Next Image")
        ,("[Home]","First Image")
        ,("[End]","Last Image")
        ,("[Shift + Left]","Move image to the previous location in the list")
        ,("[Shift + Right]","Move image to the next location in the list")
        ,("[Ctrl + <Arrow>]","Indicate Up-Direction of image and goto next")
        ,("[m]","Create directory")
        ,("[Enter]","Use date from input field and goto next image")
        ,("[Backspace]","Clear date input field")
        ,("[Delete]","Clear date")
        ,("[0-9],[space]","Enter date in format: 'day month' or 'day month year'")
        ,("[d]","Mark image for deletion")
        ]
    H.span  H.! A.id "help" $ do
      H.h4 $ H.a H.! A.href "?" H.! A.onclick "return applyChanges();" $ "Apply changes"

defContentType p = mimeByExt defaultMimeMap defaultMimeType $ T.pack p

file p = responseFile
  status200 [("Content-Type", defContentType p)]
  p Nothing


a' =~? b = do
  a <- a'
  return ( a =~ (b :: String) :: Bool)

targetDirs = fmap (toH . tC) . getDirs

toH :: [T] -> H.Html
toH [] = return ()
toH d = H.ul $ forM_ d $ \(T x cs) ->
  H.li H.! A.id (fromString x) $ do
    H.span $ H.string $ takeFileName x
    toH cs

getDirs dir = do
    cs <- filter (/= dir) . sort
      <$> find (depth ==? 0) (fileType ==? Directory) dir
    -- print cs
    return . T dir =<< mapM getDirs cs

images dir = filter (/= dir) . sort
      <$> find (depth ==? 0) ((fmap toLower <$> extension) =~? "\\.(jpg|jpeg|png)"
                             &&? (fileName /~? "delete_*") ) dir

data T = T { tP :: FilePath,
             tC :: [T]} deriving Show


main :: IO ()
main = do
  a <- getArgs
  cur <- getCurrentDirectory
  unless (length a == 2) $ do
    putStrLn "usage:\n\n\tmemis [source folder] [target folder]"
    exitFailure
  let [s,t] = a
  printf "Source: %s\nTarget: %s\nhttp://localhost:%d\n" s t port
  run port
    . logStdoutDev
    . staticPolicy (addBase $ cur </> "static")
    $ app2 t s
