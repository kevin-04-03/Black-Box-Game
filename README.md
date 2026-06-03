Haskell Black Box Simulator and Solver
This Haskell script simulates and solves the classic logic game Black Box. In this game, an n x n grid contains hidden "atoms," and the player must deduce their locations by firing "rays" from the edges of the grid and observing their trajectory.

Features
Challenge 1 (Simulator): Given a grid size and a list of atom coordinates, the program simulates firing a ray from every possible edge index and calculates the exact outcome.

Challenge 2 (Solver): Given a grid size, the total number of hidden atoms, and a list of observed ray behaviors, the program determines the exact coordinates of the hidden atoms that satisfy those conditions.

How the Puzzle Works
Rays are fired from the perimeter of the grid (North, South, East, West). They travel in a straight line but interact with atoms in the following ways:

Absorb: If a ray runs directly into an atom, it is destroyed and does not exit the grid.

Deflect (Path): If a ray passes diagonally adjacent to an atom, its path is deflected 90 degrees away from the atom. It will continue traveling and deflecting until it exits the board at a new edge and index.

Reflect: A ray is reflected directly back out of its entry point if:

It encounters a dead end (e.g., it travels directly into the pocket between two diagonally adjacent atoms).

An atom is immediately adjacent diagonally to the entry point, causing it to bounce back before entering the board.

Code Structure
Key Data Types
Pos = (Int, Int): Represents a (Column, Row) coordinate on the grid.

Side = North | East | South | West: Represents the edge of the board.

EdgePos = (Side, Int): Represents a specific entry or exit point (e.g., (North, 3) means the third column from the top).

Marking: The outcome of a fired ray. Can be Absorb, Reflect, or Path EdgePos.

Core Functions
calcBBInteractions: Simulates the board. It takes the grid size and a list of atom positions, calculating the outcome for rays fired from all edges.

solveBB: The solver. It takes the grid size, the number of atoms, and a list of known interactions, evaluating combinations to find the valid board configurations.

How to Run
Ensure you have GHC (Glasgow Haskell Compiler) installed on your machine.

Save the code into a file named BlackBox.hs.

Open your terminal and navigate to the folder containing the file.

You can execute the script directly without compiling:

Bash
runhaskell BlackBox.hs
Alternatively, for faster execution times (especially important for the brute-force solver), compile the script with optimization first:

Bash
ghc -O2 BlackBox.hs
./BlackBox
Performance Note
The solveBB function generates and verifies combinations to find the correct atom layout. Because of this brute-force approach, solving large grids with multiple atoms (e.g., an 8x8 grid with 4+ atoms) requires evaluating thousands of configurations. Compiling with -O2 is highly recommended for complex inputs.
