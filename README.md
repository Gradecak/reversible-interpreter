# The 'Fantastic' Revirsible Interpreter #

## How to run ##
from the root of the project run

```bash
stack build
stack exec interpreter <path-to-program-file>
```

I've provided a couple of program files in the source of the project that demonstrate the
interpreters capabilities. (They end with a .hit extension)

## Section 1 (5 marks) Stack project ##
yay done.

## Section 2 (15 marks) prompt user for input ##
prompting the user for input is done using the 'prompt' function.


At the start of execution of every 'exec' function (Except Seq, Try and Pass) the 'prompt' function is called.
the function will print the statement about to be executed and wait for user input.


## Section 3 (5 marks) inspect variable state ##
a user may inspect the current state of the variable by typing i <name> when prompted
the inspect function will then pattern match the current enviroment from the head of the
list of states and ookup the name entered.


if name is not found it will return 'error'


## Section 4 (10 marks) variable history ##
in order to record history we must record the changes that happen to our Environment over time
as such. the contents of the State monad has been modified such that it holds a list of tuples.
Each tuple containing an Enviroment and the Statement that resulted in this enviroment.  [(Env, Statement)]


to preform a lookup across all of the enviroments the list of tuples is extracted to a list of enviroments.
from then a partially applied lookup is mapped across the list of enviroments. this will give us back a list of
the previous states of the variable which can then be printed


## Section 5 (10 marks) step backwards ##
As mentioned in our previous section, our state records a list of envrioments and statements that were executed to get us to that
enviromet [(Env, State)]. To step backwards we must augment our current state by removing the head of the list and calling exec
with the 'new' state being the tail of our list. This means that that statement is running in the same context (Env) it ran in originally.


A user can call back multiple times each time 1 more entry will be removed from the list.


## Section 6 (5 marks) static analysis ##
In order to do static analysis we do an initial pass over the code before the user is allowed to step through.

The initial pass is ceneterd around getting information on the satates of the variables. A modified enviroment is used
such that it will record the state of each variable rather than its value. this allows us to see if a variable has been used after being initialised
or if it has been used before being initialised. The static analysis phase will notify the user of any errors and warnings is finds
when it is finished.
