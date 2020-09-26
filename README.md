# Scrabble

## Delphi Client/Server Scrabble Game using Socket communication

**Attention! This game is under development, still in alpha version.**

### How to play

You can play with as many opponents as you like.
Choose one to be the server, and the rest will be the clients that will connect to the server.
On the server, remember to allow incoming connections to the Windows firewall (usually the first time the game is opened in server mode, Windows itself will ask if you want to open the game port for external access).
The server can define the rules of the game, while waiting for all players to connect.
When all players are connected, the server can start the game.
You can chat to all players at any time using side panel.

Each player has his turn, in sequential order, indicated on the side panel.
All players will initially receive the same amount of letters.
The idea is to build words, using as many letters as possible.
The game ends when a player has no letters left.
Each time you complete your move, all other players must accept the words you entered. If any player does not accept, you will have to review your move. You can use chat to argue.
If you are unable to form a word in a move, you will automatically receive more letters in your pot.
**A move can only be completed when all words entered are complete and valid.**

### Supported languages

- English
- Portuguese Brazil

### Supported letters dictionaries

For now, the game supports letters in:
- English
- Portuguese Brazil

For each language, there is a dictionary resource file, which basically indicates all the letters of the alphabet supported and how many times each occurs.

> You can send me a dictionary in the desired language, and so we will increase support for new dictionaries.

### Required components:

- Dam: https://github.com/digao-dalpiaz/Dam

- DzSocket: https://github.com/digao-dalpiaz/DzSocket

> I'm developing in Delphi 10.3.3 Community Edition. I still don't know to specify in which previous versions it is possible to compile.

**You are welcome to submit bugs and suggestions, as well as become a tester. Please, do not hesitate to open as many issues as you like. If you want to test the game, please contact me via email on my GitHub profile.**

### Demo Screens

![Welcome screen](Images/demo_welcome.png)

![Playing screen](Images/demo_playing.png)

### Technical information

The server works on Port TCP 6631.

### To Do

- Get better sounds
- ~~Configurations screen (turn on/off sounds)~~
- ~~Review start screen center (is not considering title bar)~~
- ~~Make the score work~~
- ~~Review random letters logic~~
- ~~Include English dictionary~~
- ~~App translation to Portuguese Brazil and support any language translation~~
- Review game rules
- Behavior when user drop connection when the game is running
- ~~Password to connect~~
- ~~Allow server change settings while waiting players~~
- ~~Implement end of game~~
- Logic to check if inserted letters is according with game rules
- Better way to show that it's your turn?
- ~~Zoom function in game grid~~
- Time-out rule to a player turn?
- If language changed during game, the rules info in title bar remains in previous language
