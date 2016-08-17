from math import acos, pi


def cossim_to_angsim(x):
    """ converts cosine similarity to angular similarity """
    return 1 - acos(x) / pi


def cossim_to_angsim_check(x):
    """ check input for cossim_to_angsim """
    if min(x) < -1 or max(x) > 1:
        raise ValueError("input values are outside expected range of -1 to 1")
