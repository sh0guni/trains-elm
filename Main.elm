import TrainApp exposing (init, update, view)
import StartApp.Simple exposing (start)

main =
  start { model = init, view = view, update = update }
