# HsSnake

HsSnake is the classic ~~worm~~ snake game in a simple text-based form.
The main purpose for the game was to have a learning project in Haskell.

You should be able to clone the repository and build the project
with ```stack build``` and then run the game with  ```stack exec hssnake-exe```.
(I have only tested this under Linux.)


## Controls & playing 
Change direction with arrow keys. Press `q` to quit while in-game.
If you run into your own tail or the walls, the game is over.
The game also ends if you fail to eat the current omnom in 10 seconds.

And finally, here's what it looks like:

![Fire](screenshots/snake.gif?raw=true)

