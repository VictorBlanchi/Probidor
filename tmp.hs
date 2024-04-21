class (Monad m) => PlayerConnections m where
  connect :: m ()
  send_response :: Player -> Protocol.Response -> m ()
  receive_request :: Player -> m Protocol.Request

play :: (PlayerConnections m) => () -> m ()

--------------------------------------------------------------

type MProd a = Lwt a

instance PlayerConnections MProd where 
  ... use Socket functions

--------------------------------------------------------------

type MTest a = ReaderT [Protocol.Request] (StateT [Protocol.Response]) a

