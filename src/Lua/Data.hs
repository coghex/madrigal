module Lua.Data where
import Prelude()
import UPrelude
import System.IO ( Handle(..) )

data LuaAction = LuaActionCmdString Handle String | LuaActionNULL deriving (Show, Eq)
data LuaResult = ResLuaSuccess | ResLuaError String
               | ResLuaNULL deriving (Show, Eq)
