from sudoku_puzzles import *

def add_colour(puzzle):
    matrix = [[None] * 9 for _ in range(9)]
    for x in range(9):
        for y in range(9):
            value = puzzle[x][y]
            if value == 0: value = ""
            cell = (value, colour[x][y])
            matrix[x][y] = cell
    return matrix

class Sudoku():
    def __init__(self, puzzle):
        self.matrix = puzzle
        self.table = [[None] * 9 for _ in range(9)]
        self.cells = []
        self.groups = []
        self.lines = [] # lines are rows and columns
        self.squares = [] # lines + squares = groups
        self.overlaps = [] # Overlaps = areas where squares and lines intersect
        number = 0
        for row_number, row in enumerate(puzzle):
            for column_number, value in enumerate(row):
                new = self.Cell(self, row_number, column_number, value, number)
                number += 1
                self.table[row_number][column_number] = new
                self.cells.append(new)
        for cell in self.cells:
            cell.like_cells = cell.get_like_cells()
        self.create_rows_cols_squares_sets()
        self.create_overlaps()

    def create_overlaps(self):
        for square in self.squares:
            for line in self.lines:
                found = False
                for cell in line.cells:
                    if cell in square.cells: found = True
                if found:
                    self.Overlap(self, square, line)

    def create_rows_cols_squares_sets(self):
        for x in range(9):
            group_cells_row = []
            group_cells_column = []
            group_cells_square = []
            for cell in self.cells:
                if cell.row == x: group_cells_row.append(cell)
                if cell.column == x: group_cells_column.append(cell)
                if cell.square == x: group_cells_square.append(cell)
            self.Group(self, f"Row {x}", group_cells_row)
            self.Group(self, f"Column {x}", group_cells_column)
            self.Group(self, f"Square {x}", group_cells_square)
            self.set_group_remaining_values()

    def set_group_remaining_values(self):
        for group in self.groups:
            group.set_remaining_values()

    def print_cell(self, row, col):
        print(self.table[row][col])

    def print_groups(self, group_type):
        if group_type == "rows":
            for count, row in enumerate(self.rows):
                print("Row:", count)
                self.print_group(row)
        if group_type == "cols":
            for count, column in enumerate(self.columns):
                print("Column:", count)
                self.print_group(column)
        if group_type == "squares":
            for count, square in enumerate(self.squares):
                print("Squares:", count)
                self.print_group(square)

    def print_group(self, group_list):
        print_string = ""
        for item in group_list:
            print_string += str(item.name()) + ", "
        print(print_string[0:-1])

    def count(self):
        solved_count = 0
        potential_count = 0
        for cell in self.cells:
            if cell.value: solved_count += 1
            potential_count += len(cell.potential_values)
        return solved_count, potential_count

    def print_values(self):
        for count_row, row in enumerate(self.table):
            if count_row in [3, 6]: print("----------------------")
            row_string = ""
            for count_col, cell in enumerate(row):
                if count_col in [3, 6]: row_string += "| "
                # print("Print values:", cell, cell.value)
                if cell.value:
                    row_string += str(cell.value) + " "
                    # print("added value", row_string)
                else:
                    row_string += "  "
                    # print("added blank")
            print(row_string)
        print(f"Solved count: {self.count()}\n")

    def print_cells(self):
        for cell in self.cells: print(cell)

    def print_cells_small(self):
        text = ""
        for cell in self.cells: text += cell.name() + ", "
        print(text[0:-1])

    def remove_used_values(self):
        for cell in self.cells:
            if not cell.value:
                cell.remove_used_values()

    def only_position(self):
        for group in self.groups:
            group.only_position()

    def doubles(self):
        for group in self.groups:
            group.doubles()

    def triples(self):
        for group in self.groups:
            group.triples()

    def forced_locations(self):
        for overlap in self.overlaps:
            overlap.forced_locations()

    def solve(self):
        improving = True
        done = False
        count = 0
        while improving and not done:
            count += 1
            print(f"Loop {count}")
            solved_count_pre, potential_count_pre = self.count()
            self.remove_used_values()
            self.only_position()
            self.doubles()
            self.triples()
            self.forced_locations()
            self.print_values()
            solved_count_post, potential_count_post = self.count()
            if solved_count_post == solved_count_pre: improving = False
            if solved_count_post == 81: done = True

    class Group():
        def __init__(self, sudoku, name, cells):
            self.sudoku = sudoku
            self.name = name
            self.cells = cells
            sudoku.groups.append(self)
            if name[0:2] == "Sq": sudoku.squares.append(self)
            else: sudoku.lines.append(self)

        def __str__(self):
            text = ""
            for cell in self.cells:
                text += cell.name() + ", "
            return f"Group {self.name}: {text[0:-1]}"

        def print(self):
            print(f"\n{self.name}")
            for cell in self.cells: print(cell)

        def set_remaining_values(self):
            values = list(range(1, 10))
            for cell in self.cells:
                if cell.value:
                    if cell.value in values:
                        values.remove(cell.value)
            self.remaining_values = values

        def doubles(self):
            self.set_remaining_values()
            for x in self.remaining_values:
                for y in self.remaining_values:
                    if x >= y: continue
                    count = 0
                    cells_with_only_xy = []
                    for cell in self.cells:
                        if cell.value: continue
                        cell_only_has_xy = True
                        for value in cell.potential_values:
                            if value not in [x, y]: cell_only_has_xy = False
                        if cell_only_has_xy:
                            count += 1
                            cells_with_only_xy.append(cell)
                        if count == 2:
                            for cell in self.cells:
                                if cell.value: continue
                                if cell in cells_with_only_xy: continue
                                for v in [x, y]:
                                    if v in cell.potential_values: cell.potential_values.remove(v)

        def triples(self):
            self.set_remaining_values()
            for x in self.remaining_values:
                for y in self.remaining_values:
                    for z in self.remaining_values:
                        if x >= y or x >= z or y >= z: continue
                        count = 0
                        cells_with_only_xyz = []
                        for cell in self.cells:
                            if cell.value: continue
                            cell_only_has_xyz = True
                            for value in cell.potential_values:
                                if value not in [x, y, z]: cell_only_has_xyz = False
                            if cell_only_has_xyz:
                                count += 1
                                cells_with_only_xyz.append(cell)
                        if count == 3:
                            for cell in self.cells:
                                if cell.value: continue
                                if cell in cells_with_only_xyz: continue
                                # print(f"Triple found: {self.name}: '{x} {y} {z}'")
                                # for xxx in cells_with_only_xyz: print(xxx)
                                for v in [x, y, z]:
                                    if v in cell.potential_values:
                                        # print(f"Triple: Removing value {cell}: {v}")
                                        cell.potential_values.remove(v)

        def only_position(self):
            for x in range(1, 10):
                count = 0
                solved = False
                solution_cell = None
                for cell in self.cells:
                    if cell.value:
                        if cell.value == x: solved = True
                    else:
                        if x in cell.potential_values:
                            count += 1
                            solution_cell = cell
                if not solved and count == 1:
                    print(f"Only Value Solution: {x}. Found: {count}. Cell: {solution_cell.name()}")
                    solution_cell.value = x

    class Overlap():
        def __init__(self, sudoku, square, line):
            self.sudoku = sudoku
            self.square = square
            self.line = line
            self.calc_cells()
            self.sudoku.overlaps.append(self)

        def __str__(self):
            text = ""
            for cell in self.cells_overlap:
                text += cell.name() + ", "
            return f"{self.square.name} {self.line.name}: {text[0:-1]}"

        def forced_locations(self):
            for x in range(1, 10):
                found = False
                for cell in self.cells_square:
                    if x == cell.value or x in cell.potential_values: found = True
                if not found: # Must be in the overlap cells => cant be in the line
                    for cell in self.cells_line:
                        if x in cell.potential_values: cell.potential_values.remove(x)
            for x in range(1, 10):
                found = False
                for cell in self.cells_line:
                    if x == cell.value or x in cell.potential_values: found = True
                if not found: # Must be in the overlap cells => cant be in the square
                    for cell in self.cells_square:
                        if x in cell.potential_values: cell.potential_values.remove(x)

        def calc_cells(self):
            self.cells_square = []
            self.cells_line = []
            self.cells_overlap = []
            for cell in self.square.cells:
                if cell in self.line.cells:
                    self.cells_overlap.append(cell)
                else:
                    self.cells_square.append(cell)
            for cell in self.line.cells:
                if cell not in self.square.cells:
                    self.cells_line.append(cell)

    class Cell():
        def __init__(self, sudoku, row, column, value, number):
            self.sudoku = sudoku
            self.row = row
            self.column = column
            self.square = int(row / 3) * 3 + int(column / 3)
            self.number = number
            if value == 0:
                self.potential_values = [1, 2, 3, 4, 5, 6, 7, 8, 9]
                self.value = None
            else:
                self.value = value
                self.potential_values = [value, ]
            self.like_cells = []

        def __str__(self):
            if self.value:
                return f"Cell{self.row}{self.column} = {self.value}"
            return f"Cell{self.row}{self.column} = {self.potential_values}"

        def name(self):
            return f"Cell{self.row}{self.column}"

        def get_like_cells(self):
            self.like_cells = []
            for cell in self.sudoku.cells:
                # if self.row == 6 and self.column == 6: print(cell)
                if cell == self: continue
                if cell.row == self.row or cell.column == self.column or cell.square == self.square:
                    # if self.row == 6 and self.column == 6:
                    #     print(cell, "Added")
                    if cell not in self.like_cells:
                        self.like_cells.append(cell)
            return self.like_cells
                # else:
                #     if self.row == 6 and self.column == 6:
                #         print(cell, "Not Added")

        def remove_used_values(self):
            if self.value: return
            # print("Remove used values:", self)
            for cell in self.like_cells:
                if cell.value and cell.value in self.potential_values:
                    self.potential_values.remove(cell.value)
            if len(self.potential_values) == 1:
                self.value = self.potential_values[0]

puzzle = Sudoku(puzzle_expert)
puzzle.solve()
# print("\nLines")
# for x in puzzle.lines:
#     print(x)
# print("\nSquares")
# for x in puzzle.squares:
#     print(x)
#
# print("\nOverlaps")
# for count, x in enumerate(puzzle.overlaps):
#     print(f"{count}: {x}")
    # print("Overlap")
    # print(x.cells_overlap)
    # print("Square")
    # print(x.cells_square)
    # print("Line")
    # print(x.cells_line)

# puzzle.print_cells()
# puzzle.print_values()
# puzzle.solve()
# puzzle.set_group_remaining_values()
#
# print("Pre triples")
# for cell in puzzle.groups[5].cells:
#     print(cell)
#
# puzzle.groups[5].triples()
#
# print()
# print("Post triples")
# for cell in puzzle.groups[5].cells:
#     print(cell)


# print(puzzle.table[0][3])
# print(puzzle.table[0][5])
# print(puzzle.table[1][5])
# print(puzzle.table[2][5])


# for cell in puzzle.cells:
#     print(cell.name(), cell.square)

# puzzle.print_groups("rows")
# puzzle.print_groups("cols")
# puzzle.print_groups("squares")

# puzzle.remove_used_values()

# puzzle.print_cell(8,7)
# puzzle.print_cell(8,8)

# puzzle.groups[8].print()


# print(puzzle.table[3][2].value)
# print(puzzle.table[3][2].potential_values)


# puzzle.print_cells()

# print("Like cells of 6,6")
# for cell in puzzle.table[6][6].like_squares:
#     print(cell)

# print("\nPotential values")
# print(puzzle.table[6][6].potential_values)
