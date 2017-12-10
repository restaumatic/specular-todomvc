module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Monoid (mempty)
import Specular.Dom.Builder.Class (el, elAttr, rawHtml, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (checkbox)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, fixFRP_, never, weakDynamic_)
import Specular.FRP.Base (for)

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
initialTasks =
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
    }
mainControl _ = do
  let
    tasks = pure initialTasks

    numTasksLeft = map (Array.length <<< Array.filter (not <<< _.completed)) tasks

    anyCompletedTasks = map (not <<< Array.null <<< Array.filter _.completed) tasks

  pure
    { tasks
    , numTasksLeft
    , anyCompletedTasks
    }

mainView :: forall m. MonadWidget m
  => { tasks :: WeakDynamic (Array Task)
     , numTasksLeft :: WeakDynamic Int
     , anyCompletedTasks :: WeakDynamic Boolean
     }
  -> m Control
mainView {tasks,numTasksLeft,anyCompletedTasks} = do
  control <- elAttr "section" ("class" := "todoapp") $ do
    {newTodo} <- elAttr "header" ("class" := "header") $ do
      el "h1" $ text "todos"
      newTodoInput

		-- This section should be hidden by default and shown when there are todos
    {toggleAll,editTasks} <- elAttr "section" ("class" := "main") $ do
      {toggleAll} <- toggleAllCheckbox
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
newTodoInput = do
  rawHtml
    """
    <input class="new-todo" placeholder="What needs to be done?" autofocus>
    """

  pure { newTodo: never }

toggleAllCheckbox :: forall m. MonadWidget m
  => m { toggleAll :: Event Boolean }
toggleAllCheckbox = do
  rawHtml
    """
		<input id="toggle-all" class="toggle-all" type="checkbox">
		<label for="toggle-all">Mark all as complete</label>
    """

  pure { toggleAll: never }

taskList :: forall m. MonadWidget m
  => { tasks :: WeakDynamic (Array Task) }
  -> m { editTasks :: Event (Array Task -> Array Task) }
taskList {tasks} = do
  -- List items should get the class `editing` when editing and `completed` when marked as completed
  elAttr "ul" ("class" := "todo-list") $
    weakDynamic_ $ for tasks $ \tasks' ->
      for_ tasks' $ \task -> do
        let attrs = if task.completed then ("class" := "completed") else mempty

        elAttr "li" attrs $ do
          elAttr "div" ("class" := "view") $ do
            void $ checkbox task.completed ("class" := "toggle")
            el "label" $ text task.description
            void $ buttonOnClick (pure $ "class" := "destroy") (pure unit)

          rawHtml """<input class="edit" value="Create a TodoMVC template">"""

  pure { editTasks: never }

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
  weakDynamic_ $ for anyCompletedTasks $ \anyCompletedTasks' ->
    when anyCompletedTasks' $
      rawHtml
        """
        <button class="clear-completed">Clear completed</button>
        """

  pure { clearCompleted: never }

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
