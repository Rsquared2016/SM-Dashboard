# SM-Dashboard

# Detection of Manipulation Campaigns in Social Networks 
## Table of contents
1. [List of all files](#1-list-of-all-files)
2. [Order of Execution](#2-order-of-execution)
3. [Notes](#notes)

## 1. List of all files
| File        | description            |
|--------------------------|--------------------|
| ``util.R``| Basic libraries and functions used in the other functions of the dashboard.|
| ``ui.R`` | The definition of the graphical interface of the dashboard. Utilizes the ``util.R`` file. |
| ``server.R`` | The definition of the functionality, i.e., metrics calculation and visualization, of the dashboard. Utilizes the ``ui.R`` file. |

## 2. Order of Execution
To recreate our work the following files need to be executed in order:
1. ``util.R`` &rarr; will provide all basic functions and libraries used in the dashboard 
2. ``ui.R`` &rarr; will define the objects displayed in the dashboard 
3. ``server.R`` &rarr; will fill the objects of the UI with functionality 
This order is implicitly given, since ``server.R`` references ``ui.R`` which references ``util.R``. 

## 3. Notes 

The dashboard can be executed by executing the function ``shinyApp(...)``at the bottom of the ``server.R``file after the server, the UI as well as the utility functions were loaded. 

:warning: Please note that when the dashboard is executed, a number of warnings will appear. These are not related to any errors, but only appear due to the fact the dashboard is not filled with any data when initially opened. To fill the dashboard with data, a user has to click on on point of the line plot displaying the Tweeting activity at the top of the dashboard. 
