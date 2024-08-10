# Masyu Solver - Prolog Project

## Project Overview

This project implements a solution to the puzzle game Masyu (also known as Pearl) using Prolog, a logic programming language. The solver applies logical assertions about the game board to ensure that the rules of Masyu are adhered to, utilizing an efficient backtracking search to find valid solutions. The program is thoroughly tested and performs well on most boards, though it faces challenges with very large puzzles that offer little initial information.

## Authors

- **Jon Ingvar Skånøy**
- **Isak Killingrød**

This project was completed as part of the IKT212 course on 10th August 2024.

## Abstract

The Masyu solver in Prolog operates using a logic programming paradigm, which is particularly effective for constrained searching problems like this one. The program represents the game board as a list of tiles, with each tile defined by a set of attributes. The solver applies logical predicates to ensure that the board adheres to the rules of Masyu and employs backtracking to explore possible solutions.

## Solution Details

### Data Structure

- **Tile Representation**: Each tile is represented as a list of attributes, including its type (empty, black, or white), and the directions of lines it contains.
- **Board Representation**: The board is represented as a list of rows, where each row is a list of tiles. Edges of the board are handled by marking adjacent non-existent tiles as empty lists.

### Logical Programming

- **Predicates**: The core of the program consists of predicates that assert the truth of various conditions on the board. These predicates ensure that the board configuration adheres to the rules of Masyu.
- **Search Algorithm**: The program uses a backtracking search to explore different possible configurations of the board, applying logical predicates to prune invalid configurations early.

### Key Predicates

- **run**: The top-level predicate that orchestrates the entire solving process, including reading the input, solving the puzzles, and writing the output.
- **solve_Puzzle**: The main predicate that handles the puzzle-solving process, including setting up the board, applying rules to black and white tiles, and ensuring all lines form a single loop.
- **preventCircles**: A predicate that checks for and prevents the formation of invalid loops (circles) on the board, ensuring that only a single loop exists.

## Correctness and Testing

- **Logical Assertions**: Many of the predicates are self-evidently correct due to the logical nature of Prolog. By asserting specific conditions for each tile and board configuration, the program ensures correctness.
- **Experiments**: The program has been extensively tested on both local puzzles and a variety of test cases provided on Bamboo. The final iteration solves all 42 test puzzles in around 57 seconds, demonstrating both correctness and efficiency.

## Project Timeline

- **Initial Setup**: Early work focused on setting up the environment, defining the basic data structures, and implementing simple predicates.
- **Development and Optimization**: The project evolved through iterative improvements, focusing on optimizing the search process and ensuring correctness.
- **Final Testing and Refinement**: Extensive testing was conducted in the final stages, with particular attention to performance on large and complex puzzles.

## Reflection

### Learning Outcomes

The project provided valuable experience in working with Prolog and the logic programming paradigm. Key insights include:
- **Modularity**: The use of predicates in Prolog allows for highly modular and readable code, making it easier to test and refine individual components.
- **Logical Design**: The logical nature of Prolog predicates ensures that solutions are correct by construction, reducing the need for extensive debugging.

### Potential Improvements

While the program performs well, there are areas for future improvement:
- **Optimized Search Order**: A potential enhancement could involve reordering the search process based on the proximity of tiles to black or white tiles, which might lead to faster constraint satisfaction.
- **Early Circle Detection**: Implementing circle detection during the tile assignment process could significantly improve performance by pruning invalid configurations earlier.

## How to Run the Project

1. Clone this repository.
2. Load the Prolog files into your preferred Prolog environment (e.g., SWI-Prolog).
3. Run the `run/0` predicate to solve the puzzles.
4. The solutions will be written to the specified output files.

## Summary

In conclusion, the Prolog-based Masyu solver demonstrates the power of logic programming for solving constrained search problems. The use of logical assertions and backtracking enables the program to efficiently explore possible solutions, and extensive testing confirms its correctness and performance. This project highlights the strengths of Prolog and the value of logical design patterns in programming.

