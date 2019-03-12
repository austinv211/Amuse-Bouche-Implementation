# Name: Part3.py
# Description: Python implementation of part 3 code
# Author: Austin Vargason

from typing import Any, TypeVar, List, NewType, Mapping
from pyrsistent import PClass, field

#defining what a just is
class Just(PClass):
    a = field()

class Nothing(PClass):
    None

# defining what a maybe is
Maybe = TypeVar('Maybe', Just, Nothing)


#make an A type variable
A = TypeVar('A')

#elemIndex function
def elemIndex(element: A, values: List[A]) -> Maybe:
    try:
        result = values.index(element)
        return Just(a=result)
    except:
        return Nothing()

# make a k type variable
K = TypeVar('K')

# lookup function
def myLookup(key: K, mapInput: Mapping[K, A]) -> Maybe:
    try:
        result = mapInput[key]
        return Just(a=result)
    except:
        return Nothing()

# Part 3a
def firstOne(values: List[A]) -> A:
    try:
        result = values[0]
        return result
    except:
        raise Exception("O Noes!")

def firstOneMaybe(values: List[A]) -> Maybe:
    try:
        result = values[0]
        return Just(a=result)
    except:
        return Nothing()



