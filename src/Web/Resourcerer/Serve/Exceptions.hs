module Web.Resourcerer.Serve.Exceptions
where

import Control.Exception

data HttpException =
  ClientError |
  MalformedInput |
  Conflict |
  NotFound |
  MethodNotAllowed |
  NotAcceptable |
  UnsupportedMediaType
  deriving (Show, Eq)

instance Exception HttpException where
