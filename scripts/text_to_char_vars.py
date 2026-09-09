#!/bin/env python3

import sys

_NAMES = {
  ' ': "c_space",
  ',': "c_comma",
  "\n": "c_newline",
  "+": "c_plus",
  "-": "c_minus",
  "[": "c_open_bracket",
  "]": "c_closed_bracket",
  '0': 'c_0',
  '1': 'c_1',
  '2': 'c_2',
  '3': 'c_3',
  '4': 'c_4',
  '5': 'c_5',
  '6': 'c_6',
  '7': 'c_7',
  '8': 'c_8',
  '9': 'c_9',  
  'a': 'c_a',
  'b': 'c_b',
  'c': 'c_c',
  'd': 'c_d',
  'e': 'c_e',
  'f': 'c_f',
  'g': 'c_g',
  'h': 'c_h',
  'i': 'c_i',
  'j': 'c_j',
  'k': 'c_k',
  'l': 'c_l',
  'm': 'c_m',
  'n': 'c_n',
  'o': 'c_o',
  'p': 'c_p',
  'q': 'c_q',
  'r': 'c_r',
  's': 'c_s',
  't': 'c_t',
  'u': 'c_u',
  'v': 'c_v',
  'w': 'c_w',
  'x': 'c_x',
  'y': 'c_y',
}

def main():
    for line in sys.stdin.readlines():
        for c in line:
            print(_NAMES[c],end='')
            print(';',end='')
    print()

main()