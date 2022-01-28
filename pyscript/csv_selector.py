#! /usr/bin/env python3

import os
import sys
import copy
import argparse
import traceback
import csv


def err_assert(check, msg):
    if check is False:
        print("Error: " + msg)
        traceback.print_stack()
        sys.exit(-1)


def has_access(fname, ACCESS):
    err_assert(os.path.exists(fname),
               "File Path Does Not Exist: {}".format(fname))
    err_assert(os.access(fname, ACCESS),
               "No Permissions for File: {}".format(fname))


def hidden_ljust(csv_str, app_str):
    if "func" in app_str or "avx" in app_str or "sse" in app_str:
        csv_str += app_str.ljust(8)
    elif "align" in app_str or app_str == "0" or app_str == "3" or app_str == "5" or app_str == "127" or app_str == "255" or app_str == "256" or app_str == "4064":
        csv_str += app_str.ljust(6)
    elif "." not in app_str or "size" in app_str:
        csv_str += app_str.ljust(10)
    else:
        csv_str += app_str.ljust(12)
    return csv_str


def csv_str_append(csv_str, app_str):
    if csv_str != "":
        csv_str += ","
    #csv_str = hidden_ljust(csv_str, app_str)
    csv_str += app_str.ljust(12)
    return csv_str


all_inner_operators = ["==", ">=", "<=", ">", "<", "!="]
all_outer_operators = ["&&", "||"]
all_select_operators = ["min", "max"]


class Parser():
    def __init__(self, text):
        self.text = text
        self.tokens = []

    def parse(self):
        self.to_str(self.parse_parentheses(self.text))

    def push(self, obj, groups, depth):
        while depth:
            groups = groups[-1]
            depth -= 1
        groups.append(obj)

    def parse_parentheses(self, s):
        groups = []
        depth = 0

        try:
            for char in s:
                if char == '(':
                    self.push([], groups, depth)
                    depth += 1
                elif char == ')':
                    depth -= 1
                else:
                    self.push(char, groups, depth)
        except IndexError:
            raise ValueError('Parentheses mismatch')
        if depth > 0:
            raise ValueError('Parentheses mismatch')
        else:
            return groups

    def split_operators(self, clause):
        new_tokens = []
        for token in clause:
            token_arr_in = [token]
            token_arr_out = []
            for op in all_outer_operators:
                for t in token_arr_in:
                    t = t.replace(op, op + "---" + op)
                    token_arr_out.extend(t.split(op))
                for i in range(0, len(token_arr_out)):
                    token_arr_out[i] = token_arr_out[i].replace("---", op)
                token_arr_in = copy.deepcopy(token_arr_out)
                token_arr_out = []
            new_tokens.extend(token_arr_in)
        return new_tokens

    def to_str(self, items):
        out = ""
        for item in items:
            if isinstance(item, (str)):
                out += item
            else:
                if out != "":
                    self.tokens.append(out)
                    out = ""
                self.to_str(item)
        if out != "":
            self.tokens.append(out)


class InnerFilter():
    def __init__(self, clause):
        self.clause = clause
        self.lhs = None
        self.op = None
        self.rhs = None

        hit = False
        for op in all_inner_operators:
            if op in clause:
                hit = True
                tokens = clause.split(op)
                err_assert(
                    len(tokens) == 2, "Invalid Clause: {}".format(clause))
                self.lhs = tokens[0]
                self.op = op
                if len(tokens) == 2:
                    self.rhs = tokens[1]
                break

        if hit is False:
            self.lhs = clause
            self.op = ""
            self.rhs = self.lhs

    def apply(self, value):
        if self.op == "==":
            return value == self.rhs
        elif self.op == "!=":
            return value != self.rhs
        elif self.op == ">":
            return float(value) > float(self.rhs)
        elif self.op == "<":
            return float(value) < float(self.rhs)
        elif self.op == ">=":
            return float(value) >= float(self.rhs)
        elif self.op == "<=":
            return float(value) <= float(self.rhs)
        else:
            return True


class Filter():
    def __init__(self, clause):
        p = Parser(clause)
        p.parse()
        self.full_expr = clause
        self.inner_filters = []
        for clauses in p.tokens:
            splittable_clause = clauses
            for op in all_outer_operators:
                splittable_clause = splittable_clause.replace(op, "----")
            splittable_clause = splittable_clause.split("----")
            for split_clause in splittable_clause:
                if split_clause != "":
                    self.inner_filters.append(InnerFilter(split_clause))

        err_assert(
            len(self.inner_filters) > 0,
            "No Filters For Clause: {}".format(clause))

    def apply(self, csv_line):
        expr = self.full_expr
        for inner_filter in self.inner_filters:
            if inner_filter.apply(
                    csv_line[inner_filter.lhs].lstrip().rstrip()):
                expr = expr.replace(inner_filter.clause, "True")
            else:
                expr = expr.replace(inner_filter.clause, "False")

        expr = expr.replace("&&",
                            " and ").replace("||",
                                             " or ").replace("!", " not ")
        ret = eval(expr)
        return ret


class Redirect():
    def __init__(self, statement):
        statement_fields = statement.split("->")
        err_assert(
            len(statement_fields) <= 2,
            "Invalid Statement: {}".format(statement))
        self.field_hdr = statement_fields[0]
        self.field_value = self.field_hdr
        self.is_alias = False

        if len(statement_fields) == 2:
            self.field_value = statement_fields[1]
            self.is_alias = True

    def make_generic_key(self):
        return self.field_hdr.rstrip().lstrip()

    def make_outer_key(self, csv_line):
        if self.is_alias is False:
            return "-" + self.field_hdr.rstrip().lstrip(
            ) + self.field_value.rstrip().lstrip() + csv_line[
                self.field_value].rstrip().lstrip()
        else:
            return ""

    def make_inner_key(self, csv_line):
        if self.is_alias is False:
            return self.field_hdr.rstrip().lstrip()
        else:
            return csv_line[self.field_hdr].rstrip().lstrip(
            ) + " " + self.field_value.rstrip().lstrip()


class Selector():
    def __init__(self, clause, cols, select_delim):
        self.nop = False
        self.delim = False
        if clause == "":
            self.nop = True
            self.clause = None
            self.cols = None
            self.count = None
            self.op = None
            self.cmp_field = None
            self.cmp_results = None
            return

        self.delim = select_delim
        self.cols = cols
        for col in self.cols:
            assert col.is_alias is False, "No alias cols in selector!"
        comparitor = clause.split(":")
        assert len(comparitor) == 2 or len(
            comparitor) == 3, "Invalid clause: {}".format(clause)

        self.count = 1
        if len(comparitor) == 3:
            self.count = int(comparitor[1])

        self.op = comparitor[0]
        assert self.op in all_select_operators, "Invalid op: {}".format(
            self.op)

        self.cmp_field = comparitor[len(comparitor) - 1]

        self.cmp_results = {}

    def apply(self, new_val, old_val):
        if self.op == "min":
            return float(new_val) < float(old_val)
        elif self.op == "max":
            return float(new_val) > float(old_val)
        else:
            assert False, "Error invalid op"

    def is_selected(self, key):
        if self.cmp_field in key:
            return True
        for col in self.cols:
            if col.make_generic_key() in key:
                return True
        return False

    def add_comparison(self, key, val, cmp_key):

        pairs = self.cmp_results[cmp_key]
        for i in range(0, len(pairs)):
            assert len(pairs[i]) == 2, "Invalid Pair: {} [{}]".format(
                len(pairs[i]), str(pairs[i]))
            if self.apply(val, pairs[i][1]):
                self.cmp_results[cmp_key].insert(i, [key, val])
                return
        self.cmp_results[cmp_key].append([key, val])

    def process_results(self, results_map):
        if self.nop is True:
            return results_map

        for key in results_map:
            key_fields = key.split("-")
            cmp_key = ""
            for key_field in key_fields:
                if self.is_selected(key_field) is False:
                    cmp_key += key_field

            lmap = results_map[key]
            new_val = ""
            for lkey in lmap:
                if lkey == self.cmp_field:
                    new_val = lmap[lkey]
                    break

            assert new_val != "", "Unable to find cmp_field: {}".format(
                self.cmp_field)

            if cmp_key in self.cmp_results:
                self.add_comparison(key, new_val, cmp_key)
            else:
                self.cmp_results[cmp_key] = [[key, new_val]]

        selected_results = {}
        for cmp_key in self.cmp_results:
            pairs = self.cmp_results[cmp_key]
            bound = min(self.count, len(pairs))
            for i in range(0, bound):
                key = pairs[i][0]
                selected_results[key] = results_map[key]
                if i == (bound - 1) and self.delim:
                    selected_results[key]["delim"] = True
                else:
                    selected_results[key]["delim"] = False

        return selected_results


class Output():
    def __init__(self, rcols, col_group, delim_groups, marking, selector):
        self.cols = rcols
        self.lines = {}
        self.out_order = []
        self.out_order_map = {}
        self.col_group = col_group
        self.delim_groups = delim_groups
        self.selector = selector
        self.marking = marking

    def add_line(self, line, mark):
        key = ""
        for col in self.cols:
            key += col.make_outer_key(line)

        for col in self.cols:
            if key not in self.lines:
                self.lines[key] = {}

            inner_key = col.make_inner_key(line)
            self.lines[key][inner_key] = line[col.field_value]
            if inner_key not in self.out_order_map:
                self.out_order.append(inner_key)
                self.out_order_map[inner_key] = True
        if self.marking is True:
            self.lines[key]["mark"] = mark

    def out_hdr(self):
        out = ""
        for key in self.out_order:
            out = csv_str_append(out, key)
        print(out)

    def out(self):
        self.lines = self.selector.process_results(self.lines)
        grouped_cols = {}
        for key in self.lines:
            out = ""
            group_key = ""
            old_val = 0.0
            new_val = 0.0
            for lkey in self.out_order:
                new_info = ""
                new_info = self.lines[key][lkey]
                if "new Time" in lkey:
                    new_val = float(new_info)
                if "old Time" in lkey:
                    old_val = float(new_info)
                if self.col_group == lkey:
                    group_key = new_info
                out = csv_str_append(out, new_info)

            #out = csv_str_append(out, str(round(100 * (new_val / old_val), 1)))
            if group_key not in grouped_cols:
                grouped_cols[group_key] = []

            out = out.lstrip().rstrip()
            if self.marking is True:
                if self.lines[key]["mark"] is True:
                    out = "****" + out + "****"
                else:
                    out = "    " + out

            grouped_cols[group_key].append(out)
            if "delim" in self.lines[key] and self.lines[key]["delim"] is True:
                grouped_cols[group_key].append("-")

        for key in grouped_cols:
            if self.delim_groups is True:
                print("-------------------------")
            for line in grouped_cols[key]:
                print(line)


parser = argparse.ArgumentParser(
    description="Select fields from csv and print for table generator")

parser.add_argument("--file", action="store", default="", help="File to parse")
parser.add_argument("--col",
                    action="store",
                    default="",
                    help="Columns for table")
parser.add_argument("--group",
                    action="store",
                    default="",
                    help="Group by Cols")
parser.add_argument("--mark",
                    action="store",
                    default="",
                    help="Mark certain column conditions")
parser.add_argument("--select-cols",
                    action="store",
                    default="",
                    help="Columns to Select From")
parser.add_argument("--select-cmp",
                    action="store",
                    default="",
                    help="Comparison Operator for Selection")
parser.add_argument("--select-delim",
                    action="store_true",
                    default=False,
                    help="Comparison Operator for Selection")
parser.add_argument("--group-delim",
                    action="store_true",
                    default=False,
                    help="Seperate Groups")
parser.add_argument("--row",
                    action="store",
                    default="",
                    help="Tables to store")

args = parser.parse_args()
fname = args.file

err_assert(fname != "", "File argument required")
has_access(fname, os.R_OK)

cols = args.col.split(",")
rows = args.row.split(",")
marks = []
if args.mark != "":
    marks = args.mark.split(",")

select_cols_str = args.select_cols.split(",")
select_cmp = args.select_cmp
select_delim = args.select_delim

col_group = args.group
delim_groups = args.group_delim
col_redirects = []
row_filters = []
markers = []

select_cols = []
for select_col in select_cols_str:
    select_cols.append(Redirect(select_col))

s = Selector(select_cmp, select_cols, select_delim)

for col in cols:
    col_redirects.append(Redirect(col))

for row in rows:
    row_filters.append(Filter(row))

for mark in marks:
    markers.append(Filter(mark))

o = Output(col_redirects, col_group, delim_groups, len(marks) != 0, s)

lines_out = []
with open(fname) as csvfile:
    header = [h.strip() for h in csvfile.readline().split(',')]
    reader = csv.DictReader(csvfile, fieldnames=header)
    for line in reader:
        row_marked = False
        row_good = 0
        for row_filter in row_filters:
            if row_filter.apply(line):
                row_good += 1

        if row_good != len(row_filters):
            continue

        for marker in markers:
            if marker.apply(line):
                row_marked = True
                break
        o.add_line(line, row_marked)

o.out_hdr()
o.out()
