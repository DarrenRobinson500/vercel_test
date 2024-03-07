from django.shortcuts import render, redirect
from django.contrib import messages
from collections import Counter
from itertools import *
from .forms import *
from .sudoku_logic import *

colour = [
    [1, 1, 1, 0, 0, 0, 1, 1, 1],
    [1, 1, 1, 0, 0, 0, 1, 1, 1],
    [1, 1, 1, 0, 0, 0, 1, 1, 1],
    [0, 0, 0, 1, 1, 1, 0, 0, 0],
    [0, 0, 0, 1, 1, 1, 0, 0, 0],
    [0, 0, 0, 1, 1, 1, 0, 0, 0],
    [1, 1, 1, 0, 0, 0, 1, 1, 1],
    [1, 1, 1, 0, 0, 0, 1, 1, 1],
    [1, 1, 1, 0, 0, 0, 1, 1, 1],
]

def add_colour(puzzle):
    matrix = [[None] * 9 for _ in range(9)]
    for x in range(9):
        for y in range(9):
            value = puzzle[x][y]
            if value == 0: value = ""
            cell = (value, colour[x][y])
            matrix[x][y] = cell
    return matrix

def sudoku(request):
    puzzle = Sudoku(puzzle_template)
    # puzzle = Sudoku(puzzle_easy)
    matrix = [[None] * 9 for _ in range(9)]
    if request.method == 'POST':
        print("\nPrinting request post")
        for key, value in request.POST.items():
            print(f"{key}: {value}")
            if key[0:4] == "cell":
                x = int(key[4])
                y = int(key[5])
                if value.isdigit():
                    matrix[x][y] = int(value)
                if value is None:
                    matrix[x][y] = 0
        print(matrix)
        puzzle = Sudoku(matrix)
        puzzle.solve()

    range9 = range(9)
    puzzle.print_values()
    context = {'range9': range9, "puzzle": puzzle}
    return render(request, "sudoku.html", context)