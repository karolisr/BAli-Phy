#!/bin/sh
rm -f parser.hh parser.cc lexer.l
flex -o lexer.cc lexer.l
bison parser.y --defines=parser.hh --output=parser.cc
