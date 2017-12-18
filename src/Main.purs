module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (domEventWithSample, dynText, el, elAttr, elDynAttr, elDynAttr', rawHtml, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (checkboxView, textInput, textInputValueEventOnEnter)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, filterEvent, fixFRP, fixFRP_, foldDyn, holdDyn, leftmost, never, tagWeakDyn, weakDynamic_)
import Specular.FRP.Base (for)
import Specular.FRP.List (weakDynamicList)
import Specular.FRP.Replaceable (weakDynamic)
import Specular.FRP.WeakDynamic (switchWeakDyn)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

type NewTask =
  { description :: String
  }

type Task =
  { description :: String
  , completed :: Boolean
  }

initialTasks :: Array Task
initialTasks = join $ Array.replicate 15 $
  [ { description: "Taste PureScript", completed: true }
  , { description: "Buy a pizza", completed: false }
  ]

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = fixFRP_ $ mainView >=> mainControl

type Control =
  { newTodo :: Event NewTask
  , toggleAll :: Event Boolean
  , editTasks :: Event (Array Task -> Array Task)
  , clearCompleted :: Event Unit
  }

mainControl :: forall m. MonadFRP m
  => Control
  -> m
    { tasks :: Dynamic (Array Task)
    , numTasksLeft :: Dynamic Int
    , anyCompletedTasks :: Dynamic Boolean
    , allCompleted :: Dynamic Boolean
    }
mainControl control = do
  let
    changeTasks = leftmost
      [ map (\completed -> map (_ { completed = completed })) control.toggleAll
      , Array.filter (not <<< _.completed) <$ control.clearCompleted
      , map (\{description} tasks -> Array.snoc tasks {description, completed: false})
          control.newTodo
      , control.editTasks
      ]

  tasks <- foldDyn ($) initialTasks changeTasks

  let
    numTasksLeft = map (Array.length <<< Array.filter (not <<< _.completed)) tasks

    anyCompletedTasks = map (Array.any _.completed) tasks

    allCompleted = map (Array.all _.completed) tasks

  pure
    { tasks
    , numTasksLeft
    , anyCompletedTasks
    , allCompleted
    }

mainView :: forall m. MonadWidget m
  => { tasks :: WeakDynamic (Array Task)
     , numTasksLeft :: WeakDynamic Int
     , anyCompletedTasks :: WeakDynamic Boolean
     , allCompleted :: WeakDynamic Boolean
     }
  -> m Control
mainView {tasks,numTasksLeft,anyCompletedTasks,allCompleted} = do
  control <- elAttr "section" ("class" := "todoapp") $ do
    {newTodo} <- elAttr "header" ("class" := "header") $ do
      el "h1" $ text "todos"
      newTodoInput

		-- This section should be hidden by default and shown when there are todos
    {toggleAll,editTasks} <- elAttr "section" ("class" := "main") $ do
      {toggleAll} <- toggleAllCheckbox {allCompleted}
      {editTasks} <- taskList {tasks}
      pure {toggleAll,editTasks}

    -- This footer should hidden by default and shown when there are todos
    {clearCompleted} <- elAttr "footer" ("class" := "footer") $ do 
      itemsLeftCounter {numTasksLeft}
      filters
      {clearCompleted} <- clearCompletedButton {anyCompletedTasks}
      pure {clearCompleted}

    pure {newTodo,toggleAll,editTasks,clearCompleted}

  infoFooter

  pure control

newTodoInput :: forall m. MonadWidget m
  => m { newTodo :: Event NewTask }
newTodoInput = fixFRP $ \input -> do
  widget <- textInput
    { initialValue: ""
    , setValue: input.setValue
    , attributes: pure $
           "class" := "new-todo"
        <> "placeholder" := "What needs to be done?"
        <> "autofocus" := "autofocus"
    }

  entered <- textInputValueEventOnEnter widget

  let
    setValue = "" <$ entered
    newTodo = map (\value -> { description: value }) $ filterEvent (_ /= "") entered

  pure $ Tuple
    { setValue }
    { newTodo }

toggleAllCheckbox :: forall m. MonadWidget m
  => { allCompleted :: WeakDynamic Boolean }
  -> m { toggleAll :: Event Boolean }
toggleAllCheckbox {allCompleted} = do
  toggleAll <- checkboxView allCompleted (pure $ "class" := "toggle-all" <> "id" := "toggle-all")
  elAttr "label" ("for" := "toggle-all") $ text "Mark all as complete"

  pure { toggleAll }

taskList :: forall m. MonadWidget m
  => { tasks :: WeakDynamic (Array Task) }
  -> m { editTasks :: Event (Array Task -> Array Task) }
taskList {tasks} = do
  -- List items should get the class `editing` when editing and `completed` when marked as completed
  editTasks <- elAttr "ul" ("class" := "todo-list") $
    map (switchWeakDyn <<< map applyEdits) $ weakDynamicList tasks taskWidget

  pure { editTasks }

applyEdits :: forall a. Array (Event (a -> Maybe a)) -> Event (Array a -> Array a)
applyEdits itemEvents = leftmost $ Array.mapWithIndex (\i -> map (alterAt' i)) itemEvents

alterAt' :: forall a. Int -> (a -> Maybe a) -> Array a -> Array a
alterAt' index f array = fromMaybe array $ Array.alterAt index f array

type TaskInnerWidget =
  forall m. MonadWidget m
  => m
    { commitEdit :: Event (Task -> Maybe Task)
    , cancelEdit :: Event Unit
    , startEditing :: Event Unit
    }

taskWidget :: forall m. MonadWidget m
  => WeakDynamic Task
  -> m (Event (Task -> Maybe Task))
taskWidget wdtask = fixFRP $ \input -> do
  let attrs = lift2 (\task editing ->
                      "class" := (joinWith " " $
                                   mwhen editing ["editing"] <>
                                   mwhen task.completed ["completed"])
                    )
                    wdtask
                    input.editing

  let
    -- FIXME: This should be replaced by some Record-generic programming
    switchEvents wdyn =
      { commitEdit: switchWeakDyn $ map _.commitEdit wdyn
      , cancelEdit: switchWeakDyn $ map _.cancelEdit wdyn
      , startEditing: switchWeakDyn $ map _.startEditing wdyn
      }

    editMode :: TaskInnerWidget
    editMode = do
      map switchEvents $ weakDynamic $ for wdtask $ \task -> do
        widget <- textInput
          { initialValue: task.description
          , setValue: never
          , attributes: pure $
              "class" := "edit" <>
              "focused" := "focused"
          }

        entered <- textInputValueEventOnEnter widget
        
        pure
          { commitEdit: map (\newText t -> Just (t { description = newText })) entered
          , cancelEdit: never -- TODO: cancel on escape
          , startEditing: never
          }

    viewMode :: TaskInnerWidget
    viewMode = do
      elAttr "div" ("class" := "view") $ do
        setCompleted <- checkboxView (map _.completed wdtask) (pure $ "class" := "toggle")
        Tuple label _ <- elDynAttr' "label" (pure mempty) $ dynText $ map _.description wdtask
        delete <- buttonOnClick (pure $ "class" := "destroy") (pure unit)

        startEditing <-
          map (void <<< filterEvent (not <<< _.completed) <<< tagWeakDyn wdtask) $
          domEventWithSample (\_ -> pure unit) "dblclick" label

        pure
          { commitEdit: leftmost
              [ (\_ -> Nothing) <$ delete
              , (\completed t -> Just (t { completed = completed })) <$> setCompleted
              ]
          , cancelEdit: never
          , startEditing
          }

  {commitEdit,cancelEdit,startEditing} <- elDynAttr "li" attrs $ do
    map switchEvents $ weakDynamic $ for input.editing $ \editing ->
      if editing
        then editMode
        else viewMode

  editing <- holdDyn false $ leftmost
    [ false <$ cancelEdit
    , false <$ commitEdit
    , true <$ startEditing
    ]

  pure $ Tuple
    {editing}
    commitEdit

mwhen :: forall m. Monoid m => Boolean -> m -> m
mwhen false _ = mempty
mwhen true x = x

for2 :: forall f a b c. Applicative f => f a -> f b -> (a -> b -> c) -> f c
for2 x y f = lift2 f x y

itemsLeftCounter :: forall m. MonadWidget m
  => { numTasksLeft :: WeakDynamic Int }
  -> m Unit
itemsLeftCounter {numTasksLeft} = do
  elAttr "span" ("class" := "todo-count") $
    weakDynamic_ $ for numTasksLeft $ \numTasksLeft' ->
      rawHtml $ "<strong>" <> show numTasksLeft' <> "</strong> item left</span>"
        -- TODO: pluralization

filters :: forall m. MonadWidget m => m Unit
filters =
  -- TODO: implement routing and filters
  when false $

  rawHtml
    """
		<!-- Remove this if you don't implement routing -->
		<ul class="filters">
			<li>
				<a class="selected" href="#/">All</a>
			</li>
			<li>
				<a href="#/active">Active</a>
			</li>
			<li>
				<a href="#/completed">Completed</a>
			</li>
		</ul>
    """

clearCompletedButton :: forall m. MonadWidget m
  => { anyCompletedTasks :: WeakDynamic Boolean }
  -> m { clearCompleted :: Event Unit }
clearCompletedButton {anyCompletedTasks} = do
  -- Hidden if no completed items are left
  clearCompleted <- map switchWeakDyn $ weakDynamic $
    for anyCompletedTasks $ \anyCompletedTasks' ->
      if anyCompletedTasks'
        then
          buttonOnClick (pure ("class" := "clear-completed")) $ text "Clear completed"
        else
          pure never

  pure { clearCompleted }

infoFooter :: forall m. MonadWidget m => m Unit
infoFooter =
  rawHtml
    """
		<footer class="info">
			<p>Double-click to edit a todo</p>
			<p>Created by <a href="https://github.com/zyla">Maciej Bielecki</a></p>
			<p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
		</footer>
    """
