{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Data.List (sortOn, intercalate)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, utctDay, Day)
import Text.Read (readMaybe)

-- | Priority levels for tasks, with predefined levels High, Medium, and Low.
data Priority = High | Medium | Low deriving (Eq, Ord, Read, Show)

-- | A Task consists of an ID, description, completion status, priority, and an optional due date.
data Task = Task { taskId      :: Int        -- Unique identifier for the task
                 , description :: String     -- Description of the task
                 , completed   :: Bool       -- True if the task is completed
                 , priority    :: Priority   -- Priority level of the task
                 , dueDate     :: Maybe Day  -- Optional due date
                 } deriving (Read, Show)

-- | File to store tasks
filePath :: FilePath
filePath = "tasks.txt"

-- | Main entry point for the application. Initializes task storage and starts the main menu loop.
main :: IO ()
main = do
    putStrLn "Welcome to the Haskell To-Do List!"
    loadOrCreateFile
    mainMenu

-- | Loads tasks from the persistent file or creates the file if it doesn't exist.
loadOrCreateFile :: IO ()
loadOrCreateFile = do
    fileExists <- doesFileExist filePath
    if not fileExists then writeFile filePath "" else return ()

-- | Loads tasks from the file into memory as a list of Task objects.
loadTasks :: IO [Task]
loadTasks = do
    contents <- readFile filePath
    return $ map read (lines contents)

-- | Saves a list of Task objects to the file persistently.
saveTasks :: [Task] -> IO ()
saveTasks tasks = writeFile filePath (unlines $ map show tasks)

-- | Main menu loop offering task management options to the user.
mainMenu :: IO ()
mainMenu = do
    putStrLn "\nMain Menu:"
    putStrLn "1. Add Task"
    putStrLn "2. View Tasks"
    putStrLn "3. Mark Task as Completed"
    putStrLn "4. Edit Task Description"
    putStrLn "5. Delete Task"
    putStrLn "6. Exit"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> addTask
        "2" -> viewTasks
        "3" -> markTaskCompleted
        "4" -> editTaskDescription
        "5" -> deleteTask
        "6" -> putStrLn "Goodbye!" >> return ()
        _   -> putStrLn "Invalid choice, try again." >> mainMenu

-- | Adds a new task with user-provided description, priority, and optional due date.
addTask :: IO ()
addTask = do
    putStr "Enter task description: "
    hFlush stdout
    desc <- getLine
    putStr "Enter task priority (High, Medium, Low): "
    hFlush stdout
    prioStr <- getLine
    let prio = fromMaybe Medium (readMaybe prioStr :: Maybe Priority)
    putStr "Enter task due date (YYYY-MM-DD) or leave blank: "
    hFlush stdout
    dueDateStr <- getLine
    dueDate <- return $ readMaybe dueDateStr :: IO (Maybe Day)
    tasks <- loadTasks
    let newTask = Task { taskId = if null tasks then 1 else taskId (last tasks) + 1
                       , description = desc
                       , completed = False
                       , priority = prio
                       , dueDate = dueDate
                       }
    saveTasks (tasks ++ [newTask])
    putStrLn "Task added successfully!"
    mainMenu

-- | Displays all tasks in a formatted manner. Shows task ID, description, priority, and optional due date.
viewTasks :: IO ()
viewTasks = do
    tasks <- loadTasks
    if null tasks
        then putStrLn "No tasks found."
        else mapM_ (putStrLn . formatTask) tasks
    mainMenu

-- | Marks a task as completed based on its ID.
markTaskCompleted :: IO ()
markTaskCompleted = do
    tasks <- loadTasks
    if null tasks
        then putStrLn "No tasks found."
        else do
            putStr "Enter task ID to mark as completed: "
            hFlush stdout
            idStr <- getLine
            let idNum = readMaybe idStr :: Maybe Int
            case idNum of
                Just tid -> do
                    let updatedTasks = map (\t -> if taskId t == tid then t { completed = True } else t) tasks
                    saveTasks updatedTasks
                    putStrLn "Task marked as completed!"
                Nothing -> putStrLn "Invalid task ID."
    mainMenu

-- | Edits the description of a task based on its ID.
editTaskDescription :: IO ()
editTaskDescription = do
    tasks <- loadTasks
    if null tasks
        then putStrLn "No tasks found."
        else do
            putStr "Enter task ID to edit: "
            hFlush stdout
            idStr <- getLine
            let idNum = readMaybe idStr :: Maybe Int
            case idNum of
                Just tid -> do
                    putStr "Enter new description: "
                    hFlush stdout
                    newDesc <- getLine
                    let updatedTasks = map (\t -> if taskId t == tid then t { description = newDesc } else t) tasks
                    saveTasks updatedTasks
                    putStrLn "Task updated successfully!"
                Nothing -> putStrLn "Invalid task ID."
    mainMenu

-- | Deletes a task from the list based on its ID.
deleteTask :: IO ()
deleteTask = do
    tasks <- loadTasks
    if null tasks
        then putStrLn "No tasks found."
        else do
            putStr "Enter task ID to delete: "
            hFlush stdout
            idStr <- getLine
            let idNum = readMaybe idStr :: Maybe Int
            case idNum of
                Just tid -> do
                    let updatedTasks = filter (\t -> taskId t /= tid) tasks
                    saveTasks updatedTasks
                    putStrLn "Task deleted successfully!"
                Nothing -> putStrLn "Invalid task ID."
    mainMenu

-- | Generates a user-friendly string representation of a task for display.
formatTask :: Task -> String
formatTask (Task tid desc comp prio due) =
    show tid ++ ". " ++ desc ++ " [Priority: " ++ show prio ++ "]" ++
    (if comp then " [Completed]" else "") ++
    maybe "" (\d -> " [Due: " ++ show d ++ "]") due

