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
import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


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
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

dotDir ('/':'.':[])     = True 
dotDir ('/':'.':'.':[]) = True 
dotDir (x:[])           = False 
dotDir (x:xs)           = dotDir xs

createOneObject x = do
    dir <- doesDirectoryExist x 
    return $ object [("name", String $ last $ T.splitOn (T.pack "/") (T.pack x)), ("value", String (T.pack x)), ("type", String (T.pack $ dirType dir))] 
	where dirType b = if b then "directory" else "file"

createJson :: [FilePath] -> IO Array 
createJson (x:[]) = do 
        obj <- createOneObject x
        return $ V.singleton $ obj
createJson (x:xs) | dotDir x  = createJson xs
                  | otherwise = do
                  obj <- createOneObject x
                  rest <- createJson xs 
                  return $ obj `V.cons` rest


handleApi :: Handler App (AuthManager App) ()
handleApi = do
    path <- decodedParam "path"
    dir' <- liftIO $ getDirectoryContents $ B.toString path
    dir <- return $ map (((B.toString path)++"/")++) dir'
    json <- liftIO $ createJson dir 
    obj <- liftIO $ createOneObject ((B.toString path) ++ "/..")
    writeJSON (obj `V.cons` json)
  where decodedParam p = fromMaybe "" <$> getParam p

handleFiles :: Handler App (AuthManager App) ()
handleFiles = do
  [file] <- handleMultipart defaultUploadPolicy $ \part -> do
      content <-  liftM BS.concat EL.consume
      return content
  path <- decodedParam "path"
  liftIO $ BS.writeFile (B.toString path) file
  return ()
 where decodedParam p = fromMaybe "" <$> getParam p

handleDownload :: Handler App (AuthManager App) ()
handleDownload = do
    path <- decodedParam "path"
    serveFile (B.toString path)
  where decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/upload",   with auth handleFiles)
         --, ("/new_user", with auth handleNewUser)
         , ("/api/:path", with auth handleApi)
         , ("/download/:path",  with auth handleDownload)
         , ("",          serveDirectory "static")
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
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a

