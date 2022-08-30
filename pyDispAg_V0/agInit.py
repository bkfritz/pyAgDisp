'''
Code to reproduce AGINIT.for

AGINIT.for sets up all default data pointers for data input

The fortran code uses RECORD data structures, which are similar to Class and DataClass objects.

'''

from dataclasses import dataclass, field
import numpy as np
import math

