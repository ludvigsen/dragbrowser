{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import System.Directory
import qualified Data.Vector as V
import Data.List
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import Snap.Extras.JSON
import qualified Data.Enumerator.List as EL
import Snap.Util.FileUploads
import Control.Monad
import qualified Control.Exception as C
import System.IO.Error
import Data.String.Utils
import Data.Monoid
import System.Cmd
import System.Exit
import qualified Text.XmlHtml as X
import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


-- | A splice that can be used to check for existence of a user. If a user is
-- -- present, this will run the contents of the node.
-- --
-- -- > <ifLoggedIn> Show this when there is a logged in user </ifLoggedIn>
ifNoUserFile :: SnapletLens b (AuthManager b) -> SnapletISplice b
ifNoUserFile auth = do
    fileExists <- liftIO $ doesFileExist "users.json"
    if fileExists then
        liftIO $ putStrLn "file exists"
        else
            liftIO $ putStrLn "file does not exist"
    case fileExists of
        True -> getParamNode >>= return . X.childNodes
        False -> return []

handleIndex :: Handler App (AuthManager App) ()
handleIndex = do
    fileExists <- liftIO $ doesFileExist "users.json"
    if fileExists then
        liftIO $ putStrLn "file exists"
        else
            liftIO $ putStrLn "file does not exist"
    case fileExists of
        False -> render "first_user" --getParamNode >>= return . X.childNodes
        True -> render "index" 
    --heistLocal (I.bindSplice "ifNoUserCreated" ifNoUserCreated) $ render "index"

------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = do
        fileExists <- liftIO $ doesFileExist "users.json"
        loggedIn <- isLoggedIn
        if not fileExists || loggedIn then 
            do
                registerUser "login" "password" >> redirect "/"
            else
                redirect "/"

dotDir ('/':'.':[])     = True 
dotDir ('/':'.':'.':[]) = True 
dotDir (_:[])           = False 
dotDir (_:xs)           = dotDir xs

createOneObject x t =  do
    dir <- doesDirectoryExist x 
    return $ object [("name", String $ last $ T.splitOn (T.pack "/") (T.pack x)), ("value", String (T.pack x)), ("type", String (T.pack t))] 

createJson :: [FilePath] -> String -> IO Array 
createJson [] t     = return $ V.fromList []
createJson (x:[]) t = do 
        putStrLn $ x ++ t
        obj <- createOneObject x t
        return $ V.singleton obj
createJson (x:xs) t | dotDir x  = createJson xs t
                    | otherwise = do
                        obj <- createOneObject x t
                        rest <- createJson xs t
                        return $ obj `V.cons` rest

decodedParam p = fromMaybe "" <$> getParam p

handleDelete :: Handler App App ()
handleDelete = do
    path <- decodedParam "path"
    liftIO (removeFile (B.toString path)) 

handleMove :: Handler App App ()
handleMove = do
    from <- decodedParam "from"
    to <- decodedParam "to"
    fileExists <- liftIO $ doesFileExist (B.toString from)
    dirExists <- liftIO $ doesDirectoryExist (B.toString from)
    if dirExists then
        do liftIO (renameDirectory (B.toString from) (B.toString to))
        else do return ()
    if fileExists then 
        do liftIO (renameFile (B.toString from) (B.toString to))
        else do return ()


-- REPLACE WITH SYSTEM INDEPENDENT WAY OF DOING THIS!!
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dest = do
     system $ "cp -r " ++ src ++ " " ++ dest
     return ()

handleCopy :: Handler App App ()
handleCopy = do
    from <- decodedParam "from"
    to <- decodedParam "to"
    fileExists <- liftIO $ doesFileExist (B.toString from)
    dirExists <- liftIO $ doesDirectoryExist (B.toString from)
    if dirExists then
        do liftIO (copyDir (B.toString from) (B.toString to))
        else do return ()
    if fileExists then 
        do liftIO (copyFile (B.toString from) (B.toString to))
        else do return ()

handleApi :: Handler App App ()
handleApi = do
    path <- decodedParam "path"
    showHidden <- decodedParam "showHidden"
    dir' <- liftIO $ getDirectoryContents $ B.toString path
    dir <- return $ map (((B.toString path)++"/")++) (sort dir')
    files <- liftIO $ filterM doesFileExist dir
    dirs <- liftIO $ filterM doesDirectoryExist dir
    dirJson <- liftIO $ createJson dirs "directory"
    fileJson <- liftIO $ createJson files "file"
    obj <- liftIO $ createOneObject ((B.toString path) ++ "/..") "up"
    writeJSON $ (obj `V.cons` dirJson) V.++ fileJson


--handleFiles = requireUser handleFiles' 
handleFiles :: Handler App App ()
handleFiles = do
  [file] <- handleMultipart defaultUploadPolicy $ \part -> do
      content <-  liftM BS.concat EL.consume
      return content
  path <- decodedParam "path"
  liftIO $ BS.writeFile (B.toString path) file
  return ()

--withUser :: (AuthUser -> Handler App App ()) -> Handler App App () 
--withUser action = do 
    --with auth currentUser >>= go 
  --where 
    --go Nothing = return () 
    --go (Just u) = maybe (return ()) (\uid -> action (AuthUser (read . T.unpack $ unUid uid) (userLogin u))) (userId u) 

handleUnAuthorized :: Handler App App ()
handleUnAuthorized = redirect "/"

handleDownload :: Handler App App ()
handleDownload = do
    path <- decodedParam "path"
    serveFile (B.toString path)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/copy/:from/:to", requireUser auth handleUnAuthorized handleCopy)--with auth handleCopy)
         , ("/api/:path", requireUser auth handleUnAuthorized handleApi)
         , ("/download/:path", requireUser auth handleUnAuthorized handleDownload)
         , ("/delete/:path",  requireUser auth handleUnAuthorized handleDelete)
         , ("/move/:from/:to", requireUser auth handleUnAuthorized handleMove)
         , ("/upload", requireUser auth handleUnAuthorized handleFiles)
         , ("",   serveDirectory "static")--heistLocal (I.bindSplices [("ifNoUserFile", (ifNoUserFile auth))]) (serveDirectory "static"))
         -- , ("/", with auth (handleIndex))
        ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    --addSplices [("ifNoUserFile", liftHeist ifNoUserFile)]
    addRoutes routes
    wrapSite (\site -> ifTop (with auth (handleIndex)) <|> site ) 
    addAuthSplices h auth
    return $ App h s a

