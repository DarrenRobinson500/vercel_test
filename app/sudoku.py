# from django.shortcuts import render, redirect
# from django.contrib import messages
# from collections import Counter
# from itertools import *
# from .forms import *
#
# puzzle1 = [
#     [3,0,6,2,0,0,5,7,0],
#     [1,5,7,0,0,0,2,0,0],
#     [2,8,0,0,3,0,6,0,0],
#     [0,1,0,0,0,6,9,3,0],
#     [0,6,3,1,0,0,9,0,0],
#     [0,0,0,3,4,7,1,5,0],
#     [6,7,0,0,0,0,0,0,0],
#     [0,0,1,6,0,0,7,2,0],
#     [5,0,9,8,7,0,3,6,0],
# ]
#
# colour = [
#     [1, 1, 1, 0, 0, 0, 1, 1, 1],
#     [1, 1, 1, 0, 0, 0, 1, 1, 1],
#     [1, 1, 1, 0, 0, 0, 1, 1, 1],
#     [0, 0, 0, 1, 1, 1, 0, 0, 0],
#     [0, 0, 0, 1, 1, 1, 0, 0, 0],
#     [0, 0, 0, 1, 1, 1, 0, 0, 0],
#     [1, 1, 1, 0, 0, 0, 1, 1, 1],
#     [1, 1, 1, 0, 0, 0, 1, 1, 1],
#     [1, 1, 1, 0, 0, 0, 1, 1, 1],
# ]
#
# def add_colour(puzzle):
#     matrix = [[None] * 9 for _ in range(9)]
#     for x in range(9):
#         for y in range(9):
#             value = puzzle[x][y]
#             if value == 0: value = ""
#             cell = (value, colour[x][y])
#             matrix[x][y] = cell
#     return matrix
#
# class Sudoku():
#     def __init__(self, puzzle):
#         self.matrix = puzzle
#         table = [[None] * 9 for _ in range(9)]
#         cells = []
#         for row_number, row in enumerate(puzzle):
#             for column_number, value in enumerate(row):
#                 new = self.Cell(self, row_number, column_number, value)
#                 table[row_number][column_number] = new
#                 cells.append(new)
#
#     def remove_used_values(self):
#         for cell in self.cells:
#             if not cell.value:
#                 cell.remove_used_values()
#
#     def print_values(self):
#         for row in self.table:
#             for cell in row:
#                 row_string = ""
#                 if cell.value:
#                     row_string += cell.value + " "
#                 else:
#                     row_string += "  "
#             print(row_string)
#
#     class Cell():
#         def __init__(self, sudoku, row, column, value):
#             self.sudoku = sudoku
#             self.row = row
#             self.column = column
#             self.square = int(row / 3) * 3 + int(column)
#             if value == 0:
#                 self.potential_values = [1, 2, 3, 4, 5, 6, 7, 8, 9]
#                 self.value = None
#             else:
#                 self.value = value
#                 self.potential_values = [value, ]
#             self.like_squares = self.get_like_cells()
#
#         def get_like_cells(self):
#             like_cells = []
#             # for cell in self.sudoku.cells:
#             #     if cell == self: continue
#             #     if cell.row == self.row or cell.column == self.column or cell.square == self.square: like_cells.append(cell)
#             return like_cells
#
#         def remove_used_values(self):
#             if self.value: return
#             for cell in self.like_squares:
#                 if cell.value and cell.value in self.potential_values:
#                     self.potential_values.remove(cell.value)
#             if len(self.potential_values) == 1:
#                 self.value = self.potential_values[0]
#
# puzzle = Sudoku(puzzle1)
# puzzle.print_values()
#
#
# def sudoku(request):
#     matrix = [[None] * 9 for _ in range(9)]
#     if request.method == 'POST':
#         print("\nPrinting request post")
#         for key, value in request.POST.items():
#             print(f"{key}: {value}")
#             if key[0:4] == "cell" and value.isdigit():
#                 x = int(key[4])
#                 y = int(key[5])
#                 matrix[x][y] = int(value)
#         print(matrix)
#     range9 = range(9)
#     puzzle = add_colour(puzzle1)
#     print(puzzle)
#     context = {'range9': range9, "puzzle": puzzle}
#     return render(request, "sudoku.html", context)