import numpy as np
from math import sqrt, acos, pi


def theta(a, b):
    """ calculates the angle between two vectors, output in degrees """
    theta = acos(np.dot(a, b) / ((norm(a) * norm(b)))) * 180 / pi
    return theta


def norm(x):
    """ the L1 norm of a vector """
    x = np.array(x)
    return np.sqrt(np.dot(x, x))


def theta0(x):
    """
    calculates angle (degrees) between a vector and the origin (0,1)
    N.B only works for two dimensional vectors
    """
    theta0_check(x)
    origin = [1, 0]
    x = np.array(x)
    origin = np.array(origin)
    theta = acos(np.dot(x, origin) / norm(x) * norm(origin)) * 180 / pi
    if x[1] >= 0:
        return theta
    elif x[1] < 0:
        return 360 - theta


def theta0_check(x):
    """ check inputs for theta0 """
    if len(x) != 2:
        raise ValueError("input vector needs to have a length of 2")


def theta0_high(x):
    """
    angle between the vector and the origin
    Sets up a reference vector in high-dimensional space
    """
    origin = np.array([1] + [0]*(len(x)-1))
    x = np.array(x)
    theta = acos(np.dot(x, origin) / norm(x) * norm(origin)) * 180/pi
    return theta


def angular_similarity(a, b):
    """ the normalised angle between two vectors bounded between 0 and 1"""
    angular_similarity_check(a, b)
    a = np.array(a)
    b = np.array(b)
    similarity = (np.dot(a, b) / sqrt(np.sum(a**2) * np.sum(b**2)))
    out = ((acos(similarity) / pi))
    return out


def angular_similarity_check(a, b):
    """ check inputs for angular_similarity """
    if len(a) != len(b):
        raise ValueError("input vectors need to be the same length")
    if len(a) < 2 or len(b) < 2:
        raise ValueError("vectors need to be at least 2 elements long")


def cosine_sim_mat(x):
    """ for a given matrix, will create a cosine similarity matrix """
    # TODO make this
    pass


def cosine_sim_vector(a, b):
    """ cosine similarity between two vectors """
    cosine_sim_vector_check(a, b)
    a = np.array(a)
    b = np.array(b)
    out = np.sum(a * b) / np.sqrt(np.sum(a**2) * np.sum(b**2))
    return out


def cosine_sim_vector_check(a, b):
    """ check input for cosine_sim_vector """
    if len(a) != len(b):
        raise ValueError("input vectors need to be the same length")


def average_vector(x, fun="mean"):
    """ calculates an average vector from rows of multiple vectors """
    # TODO
    pass


def col_median(x):
    """ column medians """
    # TODO check this isn't already in pandas
    pass


def fold_180(x):
    """ constrains angles to 180 """
    fold_180_check(x)
    out = []
    for i in x:
        if i <= 180:
            out.append(i)
        elif i > 180:
            out.append(360-i)
    return out


def fold_180_check(x):
    """ check input for fold_180 """
    if min(x) < 0:
        raise ValueError("inputs values < 0")
    if max(x) > 360:
        raise ValueError("input values > 360")


def euclid_dist(a, b):
    """ euclidean distance between two vectors """
    a = np.array(a)
    b = np.array(b)
    return np.linalg.norm(a - b)
