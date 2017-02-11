import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import System.Environment

main :: IO ()
main = do
	-- grab env variable
	env <- getEnvironment
	let port = maybe 8080 read $ lookup "PORT" env
	-- and then run the app
	warp port App
