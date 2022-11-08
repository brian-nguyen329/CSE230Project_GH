# CSE230Project_GH

### Members
1. Brian Nguyen
2. Justin Nguyen
3. Brittany Trieu
4. Victor Ung

### Proposed Application and Goal
Our proposed application is a game with a TUI. The game we're trying to clone is Guitar Hero, where there will be four columns of "guitar frets" that will be running down
the terminal. The "guitar frets" can come down as either individual ones or long rows. The player will have to press (or hold) the key corresponding to that column at the 
moment it reaches the bottom. We plan on implementing random levels ranging from easy, medium, and hard difficulties. The difficulty will be expressed by the speed of 
the "guitar frets" and the number of columns with an incoming "guitar fret", making it more difficult to not only time a key correctly but pressing multiple keys at 
the same time. The player will be scored on accuracy and will be docked for pressing a key at the wrong time.

A huge factor that might make this more difficult will be timing. In case we need to accomodate timing issues, we may need to increase the hitbox for what is considered
an "accurate key press". By increasing the hitbox, we could either add a timing delay or allow an additional index to be counted as an "accurate press". An additional
feature that we would like to implement given the time would be a local co-op function. Where two users can share the same keyboard and compete on the same level to see
who is more accurate.
