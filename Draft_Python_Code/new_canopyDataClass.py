from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class canopyData:
    '''
    Holds the various canopy data parameters.
    '''
    lcanf: int = field(default=0)  # Canopy type 0=none, 1=story, 1=optical
    hcan: float = field(default=0.0, metadata={'unit':'m'}) # Canopy height