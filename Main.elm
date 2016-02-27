import Effects exposing (Never)
import TrainApp exposing (init, update, view)
import StartApp
import Task

--app =
--  start { model = init, view = view, update = update, inputs = [] }

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
