# The Game of Go in F#
I'm trying to learn more about Alpha Go Zero and came across the book [Deep Learning and the Game of Go](https://www.manning.com/books/deep-learning-and-the-game-of-go), accompanied by code in Python [here](https://github.com/maxpumperla/deep_learning_and_the_game_of_go).
This repo is my attempt to implement the same in F# while reading the book.
## Agent comparison
Currently I have implemented two types of player agents; random playing and playing with Monte Carlo tree search. The follwing table shows results of two agents playing 100 games. The Monte Carlo agent does 100 rollouts before each move.
The agents are playing in a 5x5 board, and the komi i set to 1.5.

White player | Black player | White wins | Black wins
------------ | -------------|------------|-----------
Random | Random | 59 | 41
Random | Monte Carlo | 0 | 100
Monte Carlo | Random | 99 | 1
Monte Carlo | Monte Carlo | 52 | 48
