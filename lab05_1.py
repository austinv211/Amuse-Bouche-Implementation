# Part 1

from toolz import curry, compose
from functools import reduce, wraps
from operator import add
from typing import Any, Callable, List, TypeVar

poem = "occasional clouds\none gets a rest\nfrom moon-viewing\n"

@curry
def unlines(s: [str]) -> str:
    return s[0] + '\n' + unlines(s[1:]) if len(s)>0 else ""

def lines(s: str) -> [str]:
    return s.splitlines()

def words(s: str) -> [str]:
    return s.split()

def unwords(s: [str]) -> str:
    return s[0] + " " + unwords(s[1:]) if len(s)> 0 else ""

def sort(s: [str]) -> [str]:
    return sorted(s)

def reverse(s: [str]) -> [str]:
    return[x[::-1] for x in s]

def takeTwo(s: [str]) -> [str]:
    return s[0:2] if len(s) >= 2 else s

process = compose(unlines, sort, lines)
sortLines = compose(unlines, sort, lines)
reverseLines = compose(unlines, reverse, lines)
firstTwoLines =compose(unlines, takeTwo, lines)

@curry
def byLines(f: Callable[[Any],Any], s: str) -> str:
    return unlines(f(lines(s)))
    # return compose(unlines, f, lines(s))

@curry
def indent(s: str) -> str:
    return "   " + s

def indentEachLine(s: str) -> str:
    return compose(unlines, list) (map (indent, lines(s)))

def eachLine(f: Callable[[Any],Any], s: str) -> str:
    return compose(unlines, list) (map (f,(lines(s))))

def yell(s: str) -> str:
    return s.upper() + "!!!"

def yellEachLine(s: str) -> str:
    return  eachLine(yell, s)

def eachWord(f: Callable[[Any],Any], s: str) -> str:
    return compose(unwords, list) (map(f,(words(s))))

def yellEachWord(s: str) -> str:
    return eachWord(yell, s)

def eachWordOnEachLine(f: Callable[[Any],Any], s: str) -> str:
    return (eachWord(f,s))

def yellEachWordOnEachLine(s: str) -> str:
    return (eachWordOnEachLine(yell, s))

if __name__ == '__main__':
    # import doctest
    # doctest.testmod(verbose=True)

    # print(sortLines(poem))
    # print(reverseLines(poem))
    # print(firstTwoLines(poem))
    # print(byLines(sort, poem))
    # print(byLines(reverse, poem))
    # print(byLines(takeTwo, poem))
    # print((indentEachLine(poem)))
    # print(yellEachLine(poem))
    # print(yellEachWord(poem))
    # print(eachWordOnEachLine(yell, poem))
    print(yellEachWordOnEachLine(poem))

