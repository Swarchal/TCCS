import measurements as m


def test_theta():
    assert abs(m.theta([0,1], [1,0]) - 90) < 1e-5
    assert abs(m.theta([1,1], [10,10]) - 0) < 1e-5
    assert abs(m.theta([0,1], [0, -1]) - 180) < 1e-5


def test_norm():
    assert abs(m.norm(1) - 1) < 1e-5
    assert abs(m.norm([10, 0]) - 10) < 1e-5
    assert abs(m.norm([1,1]) - 1.414213562) < 1e-5
    assert abs(m.norm([-1,-1]) - 1.414213562) < 1e-5

def test_theta0():
    assert abs(m.theta0([1,0]) - 0) < 1e-5
    assert abs(m.theta0([0,1]) - 90) < 1e-5
    assert abs(m.theta0([0,-1]) - 270) < 1e-5
    assert abs(m.theta0([-1,0]) - 180) < 1e-5


def test_theta0_high():
    assert abs(m.theta0_high([1,0,0,0]) - 0) < 1e-5
    assert m.theta0_high([1,1,1,1]) > 0
    assert m.theta0_high([-1,0,0,0]) > 0

def test_angular_similarity():
    assert abs(m.angular_similarity([1,0], [1,0]) - 0) < 1e-5
    assert abs(m.angular_similarity([1,0], [-1,0]) - 1) < 1e-5
    assert abs(m.angular_similarity([1,0], [0,1]) - 0.5) < 1e-5


def test_cosine_sim_vector():
    assert abs(m.cosine_sim_vector([1,0], [1,0]) - 1) < 1e-5
    assert abs(m.cosine_sim_vector([1,0], [0,1]) - 0) < 1e-5
    assert abs(m.cosine_sim_vector([1,0], [-1,0]) - -1) < 1e-5


def test_fold_180():
    assert m.fold_180([100, 180, 190, 350]) == [100, 180, 170, 10]
    assert m.fold_180([0, 0.5, 180, 180, 360]) == [0, 0.5, 180, 180, 0]


def test_euclid_dist():
    assert abs(m.euclid_dist([0,0], [0,0]) - 0) < 1e-5
    assert abs(m.euclid_dist([0,0], [1,1]) - 1.414213562) < 1e-5
